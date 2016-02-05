package ru.avhaliullin.exp.typed

import ru.avhaliullin.exp.ast.ASTNode

/**
  * @author avhaliullin
  */
class FnConverter(ts: TypesStore, fs: FnStore, varIdGen: VarIdGen) {
  def convert(code: Seq[ASTNode.Expression], sig: FnSignature): TypedASTNode.FnDefinition = {
    val blockContext = BlockContext(
      sig.args.map(arg => arg.name -> VarInfo(varIdGen.methodArg(arg.name), arg.tpe)).toMap,
      Map(),
      sig.args.map(arg => varIdGen.methodArg(arg.name)).toSet
    )

    val (typedBlock, bc) = convertBlock(blockContext, code)
    TypeUtils.assertAssignable(typedBlock.tpe, sig.returnType)
    TypedASTNode.FnDefinition(sig, typedBlock.code)

  }

  def convertExpression(bc: BlockContext, expr: ASTNode.Expression): (TypedASTNode.Expression, BlockContext) = {
    expr match {
      case block: ASTNode.Block =>
        val (expr, nestedCtx) = convertBlock(bc.nestedBlock(), block.exprs)
        expr -> bc.withBlockApplied(nestedCtx)

      case ASTNode.IntConst(value) => TypedASTNode.IntConst(value) -> bc
      case ASTNode.BoolConst(value) => TypedASTNode.BoolConst(value) -> bc

      case ASTNode.Variable(name) =>
        bc.getVar(name) match {
          case None => throw new RuntimeException(s"Variable $name is not defined")
          case Some(varInfo) =>
            if (!bc.assigned(varInfo.id)) {
              throw new RuntimeException(s"Variable $name can be unassigned")
            }
            TypedASTNode.VarRead(varInfo.id, varInfo.tpe) -> bc
        }

      case ASTNode.UnaryOperator(arg, op) =>
        val (typedArg, newBc) = convertExpression(bc, arg)
        TypedASTNode.UOperator(typedArg, Operator(typedArg.tpe, op)) -> newBc

      case ASTNode.BinaryOperator(l, r, op) =>
        val (typedArg1, bc1) = convertExpression(bc, l)
        val (typedArg2, bc2) = convertExpression(bc1, r)

        TypedASTNode.BOperator(typedArg1, typedArg2, Operator(typedArg1.tpe, typedArg2.tpe, op)) -> bc2

      case ASTNode.Echo(arg) =>
        val (typedArg, newBc) = convertExpression(bc, arg)
        if (typedArg.tpe == Tpe.UNIT) {
          throw new RuntimeException("Cannot apply echo to unit expression")
        }
        TypedASTNode.Echo(typedArg) -> newBc

      case ASTNode.Assignment(variable, arg) =>
        bc.getVar(variable) match {
          case None => throw new RuntimeException(s"Assignement to undefined variable $variable")
          case Some(varInfo) =>
            val (typedArg, newBc) = convertExpression(bc, arg)
            TypeUtils.assertAssignable(typedArg.tpe, varInfo.tpe)
            TypedASTNode.VarAssignment(varInfo.id, typedArg) -> newBc.assign(varInfo.id)
        }

      case ASTNode.VarDefinition(name, tpeName) =>
        if (bc.localVars.contains(name)) {
          throw new RuntimeException(s"Variable $name already defined in scope")
        }
        val tpe = ts.getPassable(tpeName)
        val varId = varIdGen.nextVar(name)
        TypedASTNode.VarDefinition(varId, tpe) -> bc.define(VarInfo(varId, tpe))

      case ASTNode.FnCall(name, args) =>
        val (typedArgs, newBc) = args.foldLeft((Vector[TypedASTNode.Expression](), bc)) {
          case ((argsHead, bc), arg) =>
            val (typedArg, newBc) = convertExpression(bc, arg)
            (argsHead :+ typedArg, newBc)
        }
        val fnSig = fs.find(name, typedArgs.map(_.tpe))
        TypedASTNode.FnCall(fnSig, typedArgs) -> newBc

      case ASTNode.IfBlock(cond, thenBlock, elseBlock) =>
        val (condTyped, afterCondCtx) = convertExpression(bc, cond)
        if (condTyped.tpe != Tpe.BOOL) {
          throw new RuntimeException(s"If condition should be boolean type expression, found ${condTyped.tpe}")
        }
        val (thenBlockTyped, branch1Ctx) = convertBlock(afterCondCtx, thenBlock)
        val (elseBlockTyped, branch2Ctx) = convertBlock(afterCondCtx, elseBlock)
        val tpe = TypeUtils.getUpperBoundType(thenBlockTyped.tpe, elseBlockTyped.tpe)
        val (thenBlockFinal, elseBlockFinal) = if (tpe == Tpe.UNIT) {
          (thenBlockTyped.mute, elseBlockTyped.mute)
        } else {
          (thenBlockTyped, elseBlockTyped)
        }
        TypedASTNode.IfExpr(condTyped, thenBlockFinal, elseBlockFinal, tpe) -> branch1Ctx.merge(branch2Ctx)

//      case ASTNode.StructInstantiation(name, args) =>
//        val struct = ts.getStruct(name)

    }
  }

  def convertBlock(bc: BlockContext, exprs: Seq[ASTNode.Expression]): (TypedASTNode.Block, BlockContext) = {
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
                           localVars: Map[String, VarInfo],
                           outerVars: Map[String, VarInfo],
                           assigned: Set[VarId]
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
      Map(),
      outerVars ++ localVars,
      assigned
    )

    def withBlockApplied(nested: BlockContext): BlockContext = {
      copy(
        assigned = nested.assigned -- nested.localVars.values.map(_.id)
      )
    }
  }

  case class VarInfo(id: VarId, tpe: Tpe)

}
