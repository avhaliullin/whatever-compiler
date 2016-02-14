package ru.avhaliullin.whatever.semantic

import ru.avhaliullin.whatever.semantic.{SemanticTreeNode => sem}
import ru.avhaliullin.whatever.syntax.{SyntaxTreeNode => syn}

/**
  * @author avhaliullin
  */
class FnConverter(ts: TypesStore, fs: FnStore, varIdGen: VarIdGen) {
  def convert(code: Seq[syn.Expression], sig: FnSignature): sem.FnDefinition = {
    val blockContext = BlockContext(
      sig.args.map(arg => arg.name -> VarInfo(varIdGen.methodArg(arg.name), arg.tpe)).toMap,
      Map(),
      sig.args.map(arg => varIdGen.methodArg(arg.name)).toSet
    )

    val (typedBlock, bc) = convertBlock(blockContext, code)
    TypeUtils.assertAssignable(typedBlock.tpe, sig.returnType)
    sem.FnDefinition(sig, typedBlock.code)

  }

  def convertExpression(bc: BlockContext, expr: syn.Expression): (sem.Expression, BlockContext) = {
    expr match {
      case block: syn.Block =>
        val (expr, nestedCtx) = convertBlock(bc.nestedBlock(), block.exprs)
        expr -> bc.withBlockApplied(nestedCtx)

      case syn.IntConst(value) => sem.IntConst(value) -> bc
      case syn.BoolConst(value) => sem.BoolConst(value) -> bc
      case syn.StringConst(value) => sem.StringConst(value) -> bc

      case syn.Variable(name) =>
        bc.getVar(name) match {
          case None => throw new RuntimeException(s"Variable $name is not defined")
          case Some(varInfo) =>
            if (!bc.assigned(varInfo.id)) {
              throw new RuntimeException(s"Variable $name can be unassigned")
            }
            sem.VarRead(varInfo.id, varInfo.tpe) -> bc
        }

      case syn.UnaryOperator(arg, op) =>
        val (typedArg, newBc) = convertExpression(bc, arg)
        sem.UOperator(typedArg, Operator(typedArg.tpe, op)) -> newBc

      case syn.BinaryOperator(l, r, op) =>
        val (typedArg1, bc1) = convertExpression(bc, l)
        val (typedArg2, bc2) = convertExpression(bc1, r)

        sem.BOperator(typedArg1, typedArg2, Operator(typedArg1.tpe, typedArg2.tpe, op)) -> bc2

      case syn.Echo(arg) =>
        val (typedArg, newBc) = convertExpression(bc, arg)
        sem.Echo(typedArg) -> newBc

      case syn.Assignment(assignee, value) =>
        val (typedValue, newBc) = convertExpression(bc, value)
        if (typedValue.tpe == Tpe.ANY) {
          throw new RuntimeException("Assignment of 'Any' is not supported yet")
        }
        assignee match {
          case syn.Variable(name) =>
            bc.getVar(name) match {
              case None => throw new RuntimeException(s"Assignment to undefined variable $assignee")
              case Some(varInfo) =>
                TypeUtils.assertAssignable(typedValue.tpe, varInfo.tpe)
                sem.VarAssignment(varInfo.id, typedValue, read = true) -> newBc.assign(varInfo.id)
            }
          case syn.FieldAccess(name, stExpr) =>
            val (typedStExpr, newBc) = convertExpression(bc, stExpr)
            typedStExpr.tpe match {
              case Tpe.Struct(sName) =>
                val st = ts.getStruct(sName)
                val field = st.fieldsMap.getOrElse(name, throw new RuntimeException(s"Type $typedStExpr doesn't have member $name"))
                TypeUtils.assertAssignable(typedValue.tpe, field.tpe)
                sem.FieldAssignment(sem.FieldAccess(field, st, typedStExpr), typedValue, true) -> newBc
              case other =>
                throw new RuntimeException(s"Type $typedStExpr doesn't have member $name")
            }
          case other =>
            throw new RuntimeException(s"Left-side part of expression is not assignable: $other")
        }

      case syn.VarDefinition(name, tpeName) =>
        if (bc.localVars.contains(name)) {
          throw new RuntimeException(s"Variable $name already defined in scope")
        }
        val tpe = ts.getAny(tpeName)
        val varId = varIdGen.nextVar(name)
        sem.VarDefinition(varId, tpe) -> bc.define(VarInfo(varId, tpe))

      case syn.VarDefinitionWithAssignment(name, rawTpeOpt, rawExpr) =>
        val (typedExpr, newBc) = convertExpression(bc, rawExpr)
        val tpeOpt = rawTpeOpt.map(ts.getAny)
        tpeOpt.foreach(TypeUtils.assertAssignable(typedExpr.tpe, _))
        val tpe = tpeOpt.getOrElse(typedExpr.tpe)
        if (tpe == Tpe.ANY) {
          throw new RuntimeException("Assignment of 'Any' is not supported yet")
        }
        if (bc.localVars.contains(name)) {
          throw new RuntimeException(s"Variable $name already defined in scope")
        }
        val varId = varIdGen.nextVar(name)
        sem.Block(Seq(
          sem.VarDefinition(varId, tpe),
          sem.VarAssignment(varId, typedExpr, true)
        ), tpe) -> newBc.define(VarInfo(varId, tpe)).assign(varId)

      case syn.FieldAccess(name, stExpr) =>
        val (typedStExpr, newBc) = convertExpression(bc, stExpr)
        typedStExpr.tpe match {
          case Tpe.Struct(sName) =>
            val st = ts.getStruct(sName)
            val field = st.fieldsMap.getOrElse(name, throw new RuntimeException(s"Type $typedStExpr doesn't have member $name"))
            sem.FieldAccess(field, st, typedStExpr) -> newBc
          case other =>
            throw new RuntimeException(s"Type $typedStExpr doesn't have member $name")
        }

      case syn.FnCall(name, args) =>
        val (typedArgs, newBc) = args.foldLeft((Vector[sem.Expression](), bc)) {
          case ((argsHead, bc), arg) =>
            val (typedArg, newBc) = convertExpression(bc, arg)
            (argsHead :+ typedArg, newBc)
        }
        val fnSig = fs.find(name, typedArgs.map(_.tpe))
        sem.FnCall(fnSig, typedArgs) -> newBc

      case syn.IfBlock(cond, thenBlock, elseBlock) =>
        val (condTyped, afterCondCtx) = convertExpression(bc, cond)
        if (condTyped.tpe != Tpe.BOOL) {
          throw new RuntimeException(s"If condition should be boolean type expression, found ${condTyped.tpe}")
        }
        val (thenBlockTyped, branch1Ctx) = convertBlock(afterCondCtx, thenBlock)
        val (elseBlockTyped, branch2Ctx) = convertBlock(afterCondCtx, elseBlock)
        val tpe = TypeUtils.getUpperBoundType(thenBlockTyped.tpe, elseBlockTyped.tpe)
        sem.IfExpr(condTyped, thenBlockTyped, elseBlockTyped, tpe) -> branch1Ctx.merge(branch2Ctx)

      case syn.StructInstantiation(name, args) =>
        import syn.Argument._
        val struct = ts.getStruct(name)
        if (args.size != struct.fields.size) {
          throw new RuntimeException(s"Cannot instantiate structure $name - expected ${struct.fields.size} arguments, passed ${args.size}")
        }
        val (byNameExprs, _) = args.zipWithIndex.foldLeft((IndexedSeq[syn.Argument.ByName](), false)) {
          case ((head, byName), (it, idx)) =>
            it match {
              case ByOrder(value) =>
                if (byName) {
                  throw new RuntimeException(s"You cannot use by-order args after by-name args")
                }
                (head :+ ByName(struct.fields(idx).name, value)) -> false
              case bName: ByName =>
                (head :+ bName) -> true
            }
        }

        val name2Expr = byNameExprs.zipWithIndex.map {
          case (ByName(name, e), i) => name ->(e, i)
        }.toMap
        val (exprs, evalOrder) = struct.fields.foldLeft((IndexedSeq[syn.Expression](), Seq[Int]())) {
          case ((exprs, order), field) =>
            val (expr, invocationIdx) = name2Expr.getOrElse(field.name, throw new RuntimeException(s"Field ${field.name} wasn't assigned"))
            (exprs :+ expr, order :+ invocationIdx)
        }
        val (typedExprs, newCtx) = exprs.zip(struct.fields).foldLeft((Seq[sem.Expression](), bc)) {
          case ((head, ctx), (it, f)) =>
            val (e, newCtx) = convertExpression(ctx, it)
            if (!TypeUtils.isAssignable(e.tpe, f.tpe)) {
              throw new RuntimeException(s"Initializing field ${f.name} of struct ${struct.name} with incompatible type ${e.tpe} - expected ${f.tpe}")
            }
            (head :+ e, newCtx)
        }
        sem.StructureInstantiation(struct, typedExprs.toIndexedSeq, evalOrder) -> newCtx

      case syn.ArrayInstantiation(tpe, exprs) =>
        val elemTpeOpt = tpe.map(ts.getAny)
        val (args, newBc) = exprs.foldLeft((Seq[sem.Expression](), bc)) {
          case ((resExprs, bc), it) =>
            val (res, newBc) = convertExpression(bc, it)
            (resExprs :+ res, newBc)
        }
        val elemTpe = elemTpeOpt match {
          case None =>
            if (args.isEmpty) {
              throw new RuntimeException("Cannot infer type for empty array")
            }
            args.map(_.tpe).reduce(TypeUtils.getUpperBoundType)
          case Some(elem) =>
            args.map(_.tpe).foreach(TypeUtils.assertAssignable(_, elem))
            elem
        }
        if (elemTpe == Tpe.ANY) {
          throw new RuntimeException("Arrays of 'Any' type are not supported yet")
        }
        sem.ArrayInstantiation(elemTpe, args) -> newBc
    }
  }

  def convertBlock(bc: BlockContext, exprs: Seq[syn.Expression]): (sem.Block, BlockContext) = {
    if (exprs.isEmpty) {
      sem.Block(Nil, Tpe.UNIT) -> bc
    } else {
      val body = exprs.dropRight(1)
      val last = exprs.last
      val (typedExprs, newCtx) = body.foldLeft((Vector[sem.Expression](), bc)) {
        case ((head, ctx), raw) =>
          val (typedExpr, newCtx) = convertExpression(ctx, raw)
          (head :+ typedExpr.mute, newCtx)
      }
      val (lastTyped, lastCtx) = convertExpression(newCtx, last)
      val tpe = lastTyped.tpe
      sem.Block(typedExprs :+ lastTyped, tpe) -> lastCtx
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
