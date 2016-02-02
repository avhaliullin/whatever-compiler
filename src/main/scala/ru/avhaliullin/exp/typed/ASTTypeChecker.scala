package ru.avhaliullin.exp.typed

import ru.avhaliullin.exp.ast.{ASTNode => RawAst}

/**
  * @author avhaliullin
  */
object ASTTypeChecker {
  private def convertSignature(sig: RawAst.FnDefinition.Signature): TypedASTNode.FnDefinition.Signature = {
    TypedASTNode.FnDefinition.Signature(
      sig.name,
      sig.args.map(arg => TypedASTNode.FnDefinition.Arg(arg.name, Tpe(arg.tpe))),
      Tpe(sig.returnT)
    )
  }

  private def generateAndCheckSignatures(raw: Seq[RawAst.Definition]): Map[String, Set[TypedASTNode.FnDefinition.Signature]] = {
    val name2Fns = raw.collect {
      case RawAst.FnDefinition(sig, _) => convertSignature(sig)
    }.groupBy(_.name)

    name2Fns.mapValues {
      overloaded =>
        overloaded.foldLeft(Set[TypedASTNode.FnDefinition.Signature]()) {
          (acc, it) =>
            if (acc(it)) {
              throw new RuntimeException(s"Duplicated method with signature $it")
            } else {
              acc + it
            }
        }
    }
  }

  case class GlobalContext(
                            sigs: Map[String, Set[TypedASTNode.FnDefinition.Signature]],
                            blockIdGen: BlockId.Gen
                          ) {
    def findFunction(name: String, args: Seq[Tpe]): TypedASTNode.FnDefinition.Signature = {
      sigs.getOrElse(name, Set()).find {
        sig =>
          sig.args.size == args.size && sig.args.map(_.tpe).zip(args).forall {
            case (expect, actual) => checkAssignable(expect, actual)
          }
      }.getOrElse(throw new RuntimeException(s"Cannot find function $name with such signature"))
    }
  }

  def convert(raw: Seq[RawAst]): Seq[TypedASTNode.Definition] = {
    val defs = raw.collect {
      case d: RawAst.Definition => d
    }
    val signatures = generateAndCheckSignatures(defs)
    val exprs = raw.collect {
      case e: RawAst.Expression => e
    }
    val gCtx = GlobalContext(signatures, new BlockId.Gen)
    val (mainBlock, _) = convertBlock(BlockContext(gCtx, Map(), Map(), Set(), gCtx.blockIdGen.next), exprs)

    TypedASTNode.Main(mainBlock.code) +: defs.map {
      case fn: RawAst.FnDefinition =>
        convertFn(gCtx, fn)
    }
  }

  def convertFn(gCtx: GlobalContext, fn: RawAst.FnDefinition): TypedASTNode.FnDefinition = {
    val sig = convertSignature(fn.signature)

    val blockId = gCtx.blockIdGen.next
    val blockContext = BlockContext(
      gCtx,
      sig.args.map(arg => arg.name -> VarInfo(VarId(arg.name, BlockId.MethodArg), arg.tpe)).toMap,
      Map(),
      sig.args.map(arg => VarId(arg.name, BlockId.MethodArg)).toSet,
      blockId
    )

    val (typedBlock, bc) = convertBlock(blockContext, fn.code)
    assertAssignable(sig.returnType, typedBlock.tpe)
    TypedASTNode.FnDefinition(sig, typedBlock.code)
  }

  def checkAssignable(to: Tpe, from: Tpe): Boolean = {
    to == Tpe.UNIT || to == from
  }

  def assertAssignable(expected: Tpe, actual: Tpe): Unit = {
    if (!checkAssignable(expected, actual)) {
      throw new RuntimeException(s"Type check failed: expected $expected, actual $actual")
    }
  }

  def getUpperBoundType(t1: Tpe, t2: Tpe): Tpe = {
    if (t1 == t2) {
      t1
    } else {
      Tpe.UNIT
    }
  }

  def convertExpression(bc: BlockContext, expr: RawAst.Expression): (TypedASTNode.Expression, BlockContext) = {
    expr match {
      case block: RawAst.Block =>
        val (expr, nestedCtx) = convertBlock(bc.nestedBlock(), block.exprs)
        expr -> bc.withBlockApplied(nestedCtx)

      case RawAst.IntConst(value) => TypedASTNode.IntConst(value) -> bc
      case RawAst.BoolConst(value) => TypedASTNode.BoolConst(value) -> bc

      case RawAst.Variable(name) =>
        bc.getVar(name) match {
          case None => throw new RuntimeException(s"Variable $name is not defined")
          case Some(varInfo) =>
            if (!bc.assigned(varInfo.id)) {
              throw new RuntimeException(s"Variable $name can be unassigned")
            }
            TypedASTNode.VarRead(varInfo.id, varInfo.tpe) -> bc
        }

      case RawAst.UnaryOperator(arg, op) =>
        val (typedArg, newBc) = convertExpression(bc, arg)
        TypedASTNode.UOperator(typedArg, Operator(typedArg.tpe, op)) -> newBc

      case RawAst.BinaryOperator(l, r, op) =>
        val (typedArg1, bc1) = convertExpression(bc, l)
        val (typedArg2, bc2) = convertExpression(bc1, r)

        TypedASTNode.BOperator(typedArg1, typedArg2, Operator(typedArg1.tpe, typedArg2.tpe, op)) -> bc2

      case RawAst.Echo(arg) =>
        val (typedArg, newBc) = convertExpression(bc, arg)
        if (typedArg.tpe == Tpe.UNIT) {
          throw new RuntimeException("Cannot apply echo to unit expression")
        }
        TypedASTNode.Echo(typedArg) -> newBc

      case RawAst.Assignment(variable, arg) =>
        bc.getVar(variable) match {
          case None => throw new RuntimeException(s"Assignement to undefined variable $variable")
          case Some(varInfo) =>
            val (typedArg, newBc) = convertExpression(bc, arg)
            assertAssignable(varInfo.tpe, typedArg.tpe)
            TypedASTNode.VarAssignment(varInfo.id, typedArg) -> newBc.assign(varInfo.id)
        }

      case RawAst.VarDefinition(name, tpeName) =>
        if (bc.localVars.contains(name)) {
          throw new RuntimeException(s"Variable $name already defined in scope")
        }
        val tpe = Tpe(tpeName)
        val varId = VarId(name, bc.blockId)
        TypedASTNode.VarDefinition(varId, tpe) -> bc.define(VarInfo(varId, tpe))

      case RawAst.FnCall(name, args) =>
        val (typedArgs, newBc) = args.foldLeft((Vector[TypedASTNode.Expression](), bc)) {
          case ((argsHead, bc), arg) =>
            val (typedArg, newBc) = convertExpression(bc, arg)
            (argsHead :+ typedArg, newBc)
        }
        val fnSig = bc.gCtx.findFunction(name, typedArgs.map(_.tpe))
        TypedASTNode.FnCall(fnSig, typedArgs) -> newBc

      case RawAst.IfBlock(cond, thenBlock, elseBlock) =>
        val (condTyped, afterCondCtx) = convertExpression(bc, cond)
        if (condTyped.tpe != Tpe.BOOL) {
          throw new RuntimeException(s"If condition should be boolean type expression, found ${condTyped.tpe}")
        }
        val (thenBlockTyped, branch1Ctx) = convertBlock(afterCondCtx, thenBlock)
        val (elseBlockTyped, branch2Ctx) = convertBlock(afterCondCtx, elseBlock)
        val tpe = getUpperBoundType(thenBlockTyped.tpe, elseBlockTyped.tpe)
        val (thenBlockFinal, elseBlockFinal) = if (tpe == Tpe.UNIT) {
          (thenBlockTyped.mute, elseBlockTyped.mute)
        } else {
          (thenBlockTyped, elseBlockTyped)
        }
        TypedASTNode.IfExpr(condTyped, thenBlockFinal, elseBlockFinal, tpe) -> branch1Ctx.merge(branch2Ctx)
    }
  }

  def convertBlock(bc: BlockContext, exprs: Seq[RawAst.Expression]): (TypedASTNode.Block, BlockContext) = {
    if (exprs.isEmpty) {
      TypedASTNode.Block(Nil, Tpe.UNIT) -> bc
    } else {
      val body = exprs.dropRight(1)
      val last = exprs.last
      val (typedExprs, newCtx) = body.foldLeft((Vector[TypedASTNode.Expression](), bc)) {
        case ((head, ctx), raw) =>
          val (typedExpr, newCtx) = convertExpression(ctx, raw)
          (head :+ typedExpr.mute, newCtx)
      }
      val (lastTyped, lastCtx) = convertExpression(newCtx, last)
      val tpe = lastTyped.tpe
      TypedASTNode.Block(typedExprs :+ lastTyped, tpe) -> lastCtx
    }
  }

  case class BlockContext(
                           gCtx: GlobalContext,
                           localVars: Map[String, VarInfo],
                           outerVars: Map[String, VarInfo],
                           assigned: Set[VarId],
                           blockId: BlockId
                         ) {
    def merge(that: BlockContext): BlockContext = {
      copy(assigned = this.assigned.intersect(that.assigned))
    }

    def getVar(name: String): Option[VarInfo] = {
      localVars.get(name).orElse(outerVars.get(name))
    }

    def assign(varId: VarId) = copy(assigned = assigned + varId)

    def define(varInfo: VarInfo) = copy(localVars = localVars + (varInfo.id.name -> varInfo))

    def nestedBlock(): BlockContext = BlockContext(
      gCtx,
      Map(),
      outerVars ++ localVars,
      assigned,
      gCtx.blockIdGen.next
    )

    def withBlockApplied(nested: BlockContext): BlockContext = {
      copy(
        assigned = nested.assigned -- nested.localVars.values.map(_.id)
      )
    }
  }

  case class VarInfo(id: VarId, tpe: Tpe)

}
