package ru.avhaliullin.whatever.semantic.function

import ru.avhaliullin.whatever.frontend.syntax.SyntaxTreeNode.{QualifiedName, TypeExpression}
import ru.avhaliullin.whatever.frontend.syntax.{SyntaxTreeNode => syn}
import ru.avhaliullin.whatever.semantic.module.{ModuleAPI, ModuleName}
import ru.avhaliullin.whatever.semantic.tpe.{Tpe, TypeUtils}
import ru.avhaliullin.whatever.semantic.{SemanticTreeNode => sem, _}

/**
  * @author avhaliullin
  */
class FnConverter(ic: ImportsContext, modules: Map[ModuleName, ModuleAPI], varIdGen: VarIdGen) {
  private def getModule(name: ModuleName): ModuleAPI = modules.getOrElse(name, throw new RuntimeException(s"Module $name not found"))

  private def getStructure(udt: Tpe.UDT): Structure = {
    val module = getModule(udt.module)
    module.structs.getOrElse(udt.name, throw new RuntimeException(s"Structure ${udt.name} not found in module ${udt.module}"))
  }

  private def getStructure(te: syn.TypeExpression): Structure = getStructure(te.name)

  private def getStructure(qn: QualifiedName): Structure = {
    val (name, moduleName) = ic.resolveName(qn)
    val module = getModule(moduleName)
    module.structs.getOrElse(name, throw new RuntimeException(s"Structure $name not found in module $moduleName"))
  }

  private def getFunction(qn: QualifiedName, argTypes: Seq[Tpe]): FnSignature = {
    val (name, moduleName) = ic.resolveName(qn)
    val module = getModule(moduleName)
    val result = module.functions.getOrElse(name, throw new RuntimeException(s"Function $name not found in module $moduleName"))
    assertArgs(argTypes, result)
    result
  }

  private def getStructMethod(st: Structure, name: String, argTypes: Seq[Tpe]): FnSignature = {
    val fn = getModule(st.fullTpe.module)
      .structs(st.fullTpe.name)
      .methods
      .getOrElse(name, throw new RuntimeException(s"Method $name with args $argTypes not defined for type ${st.fullTpe}"))
    assertArgs(argTypes, fn)
    fn
  }

  private def assertArgs(args: Seq[Tpe], fn: FnSignature): Unit = {
    args.zip(fn.args.map(_.tpe)).foreach {
      case (from, to) => TypeUtils.assertAssignable(from, to)
    }
  }

  private def getType(te: TypeExpression): Tpe = Tpe.getTpe(te, ic)

  def convertImpl(fn: syn.FnDefinition, forType: TypeExpression): sem.FnDefinition = {
    val sig = FnAnalyzer.convertSignature(fn.signature, ic)
    val code = fn.code

    val args = FnSignature.Arg("this", getType(forType)) +: sig.args
    val blockContext = BlockContext(
      args.map(arg => arg.name -> VarInfo(varIdGen.methodArg(arg.name), arg.tpe, true)).toMap,
      Map(),
      args.map(arg => varIdGen.methodArg(arg.name)).toSet
    )

    val (typedBlock, bc) = convertBlock(blockContext, code)
    TypeUtils.assertAssignable(typedBlock.tpe, sig.returnType)
    sem.FnDefinition(sig, typedBlock.code)
  }

  def convert(fn: syn.FnDefinition): sem.FnDefinition = {
    val sig = FnAnalyzer.convertSignature(fn.signature, ic)
    val code = fn.code

    val blockContext = BlockContext(
      sig.args.map(arg => arg.name -> VarInfo(varIdGen.methodArg(arg.name), arg.tpe, true)).toMap,
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
                if (!varInfo.mutable) {
                  throw new RuntimeException(s"Variable $assignee is immutable")
                }
                TypeUtils.assertAssignable(typedValue.tpe, varInfo.tpe)
                sem.VarAssignment(varInfo.id, typedValue, read = true) -> newBc.assign(varInfo.id)
            }
          case syn.FieldAccess(name, stExpr) =>
            val (typedStExpr, newBc) = convertExpression(bc, stExpr)
            typedStExpr.tpe match {
              case udt: Tpe.UDT =>
                val st = getStructure(udt)
                val field = st.fieldsMap.getOrElse(name, throw new RuntimeException(s"Type $typedStExpr doesn't have member $name"))
                TypeUtils.assertAssignable(typedValue.tpe, field.tpe)
                sem.FieldAssignment(sem.FieldAccess(field, st, typedStExpr), typedValue, read = true) -> newBc
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
        val tpe = getType(tpeName)
        val varId = varIdGen.nextVar(name)
        sem.VarDefinition(varId, tpe) -> bc.define(VarInfo(varId, tpe, true))

      case syn.VarDefinitionWithAssignment(name, rawTpeOpt, rawExpr) =>
        val (typedExpr, newBc) = convertExpression(bc, rawExpr)
        val tpeOpt = rawTpeOpt.map(getType)
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
        ), tpe) -> newBc.define(VarInfo(varId, tpe, true)).assign(varId)

      case syn.FieldAccess(name, stExpr) =>
        val (typedStExpr, newBc) = convertExpression(bc, stExpr)
        typedStExpr.tpe match {
          case udt: Tpe.UDT =>
            val st = getStructure(udt)
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
        val fnSig = getFunction(name, typedArgs.map(_.tpe))
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
        val struct = getStructure(name)
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
              throw new RuntimeException(s"Initializing field ${f.name} of struct ${struct.fullTpe} with incompatible type ${e.tpe} - expected ${f.tpe}")
            }
            (head :+ e, newCtx)
        }
        sem.StructureInstantiation(struct, typedExprs.toIndexedSeq, evalOrder) -> newCtx

      case syn.ArrayInstantiation(tpe, exprs) =>
        val elemTpeOpt = tpe.map(getType)
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

      case syn.MethodCall(ths, name, args) =>
        val (thsExpr, newBc) = convertExpression(bc, ths)
        val (argExprs, newBc2) = args.foldLeft((Seq[sem.Expression](), newBc)) {
          case ((exprsAcc, bc), e) =>
            val (newExpr, newBc) = convertExpression(bc, e)
            (exprsAcc :+ newExpr) -> newBc
        }
        val resExpr = thsExpr.tpe match {
          case arrTpe@Tpe.Arr(elemTpe) =>
            (name, argExprs) match {
              case ("size", Seq()) => sem.ArrayLength(thsExpr)
              case ("get", Seq(index)) if TypeUtils.isAssignable(index.tpe, Tpe.INT) => sem.ArrayGet(thsExpr, index, elemTpe)
              case ("set", Seq(index, value)) if TypeUtils.isAssignable(index.tpe, Tpe.INT) && TypeUtils.isAssignable(value.tpe, elemTpe) =>
                sem.ArraySet(thsExpr, index, value)
              case _ =>
                throw new RuntimeException(s"Not found method $name with args ${argExprs.map(_.tpe).mkString(",")} of type $arrTpe")
            }
          case udt: Tpe.UDT =>
            val st = getStructure(udt)
            val method = getStructMethod(st, name, argExprs.map(_.tpe))
            sem.StructMethodCall(st, method, thsExpr, argExprs)
          case other =>
            throw new RuntimeException(s"Type $other doesn't have method $name")
        }
        resExpr -> newBc2

      case syn.ForLoop(itVarName, iterable, body) =>
        val (iterableExpr, bc1) = convertExpression(bc, iterable)
        val itVarId = varIdGen.nextVar(itVarName)
        iterableExpr.tpe match {
          case arrTpe@Tpe.Arr(elemTpe) =>
            val (bodyExpr, bc2) = convertBlock(bc1.nestedBlock().defineAssigned(VarInfo(itVarId, elemTpe, false)), body.exprs)
            sem.ForLoop(itVarId, iterableExpr, bodyExpr.mute.code) -> bc1 // Drop bc2, because i cannot prove that loop body were executed at least once
          case other => throw new RuntimeException(s"Cannot iterate over $other")
        }
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

    def defineAssigned(varInfo: VarInfo) = define(varInfo).copy(assigned = assigned + varInfo.id)

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

  case class VarInfo(id: VarId, tpe: Tpe, mutable: Boolean)

}
