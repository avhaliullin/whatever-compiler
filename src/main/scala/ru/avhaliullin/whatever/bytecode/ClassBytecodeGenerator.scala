package ru.avhaliullin.whatever.bytecode

import org.apache.bcel.Constants
import org.apache.bcel.classfile.JavaClass
import org.apache.bcel.generic._
import ru.avhaliullin.whatever.common.ClassName
import ru.avhaliullin.whatever.semantic.{SemanticTreeNode => sem, _}

/**
  * @author avhaliullin
  */
class ClassBytecodeGenerator(className: ClassName, structs: Seq[Structure]) {

  private val structName2Struct = structs.map(st => st.name -> st).toMap
  private val jtg = new JavaTypeGen(className)

  private case class MethodContext(
                                    cg: ClassGen,
                                    cpg: ConstantPoolGen,
                                    instF: InstructionFactory,
                                    mg: MethodGen,
                                    il: InstructionList
                                  ) {
    private var cpv = 0

    var vars: Map[VarId, LocalVariableGen] = mg.getLocalVariables.map {
      lvg =>
        VarId.MethodArg(lvg.getName) -> lvg
    }.toMap

    def defineVar(id: VarId, tpe: Tpe): Unit = {
      vars += id -> mg.addLocalVariable(id.name, jtg.toJavaType(tpe), null, null)
    }

    def incrementVar(id: VarId, factor: Int): InstructionHandle = {
      val lvg = vars(id)
      val ih = il.append(new IINC(lvg.getIndex, factor))
      lvg.setEnd(ih)
      ih
    }

    def storeVar(id: VarId, tpe: Tpe): InstructionHandle = {
      assignVar(id, InstructionFactory.createStore(jtg.toJavaType(tpe), _))
    }

    def assignVar(id: VarId, initializer: Int => Instruction): InstructionHandle = {
      val v = vars(id)
      val inst = initializer(v.getIndex)
      val ih = il.append(inst)
      if (v.getStart == null) {
        v.setStart(ih)
      }
      ih
    }

    def loadVar(id: VarId, tpe: Tpe): InstructionHandle = {
      val lvg = vars(id)
      val ih = il.append(InstructionFactory.createLoad(jtg.toJavaType(tpe), lvg.getIndex))
      lvg.setEnd(ih)
      ih
    }

    def withIL(il: InstructionList): MethodContext = {
      val res = copy(il = il)
      res.vars = vars
      res
    }

    def PUSH(i: Int) = new PUSH(cpg, i)

    def generateLocalVar(tpe: Tpe): VarId = {
      cpv += 1
      val id = VarId.CompilerProduced(cpv)
      defineVar(id, tpe)
      id
    }
  }

  def generateBranching(ctx: MethodContext, bInst: BranchInstruction, onPassOpt: Option[InstructionList], onBranchOpt: Option[InstructionList]): Unit = {
    (onPassOpt, onBranchOpt) match {
      case (None, None) =>
        ctx.il.append(bInst)
        val nop = ctx.il.append(new NOP)
        bInst.setTarget(nop)
      case (Some(onPass), Some(onBranch)) =>
        ctx.il.append(bInst)
        ctx.il.append(onPass)
        val jmp = ctx.il.append(new GOTO(null))
        val p0 = ctx.il.append(onBranch)
        val end = ctx.il.append(new NOP)
        bInst.setTarget(p0)
        jmp.setTarget(end)
      case (Some(onPass), None) =>
        ctx.il.append(bInst)
        ctx.il.append(onPass)
        val end = ctx.il.append(new NOP)
        bInst.setTarget(end)
      case (None, Some(onBranch)) =>
        bInst match {
          case ifInst: IfInstruction =>
            generateBranching(ctx, ifInst.negate(), onBranchOpt, onPassOpt)
          case _ =>
            ctx.il.append(bInst)
            val jmp = ctx.il.append(new GOTO(null))
            val p0 = ctx.il.append(onBranch)
            val end = ctx.il.append(new NOP)
            bInst.setTarget(p0)
            jmp.setTarget(end)
        }
    }
  }

  def generateBoolExpr(ctx: MethodContext, arg1: sem.Expression, arg2: sem.Expression, bInst: BranchInstruction): Unit = {
    generateForNode(ctx, arg1)
    generateForNode(ctx, arg2)
    generateBranching(ctx, bInst, Some(new InstructionList(ctx.PUSH(0))), Some(new InstructionList(ctx.PUSH(1))))
  }

  def generateIf(ctx: MethodContext, cond: sem.Expression, thenIl: Option[InstructionList], elseIl: Option[InstructionList]): Unit = {
    cond match {
      case BranchInliner(inliner) =>
        inliner.inline(ctx, thenIl, elseIl)

      case _ =>
        generateForNode(ctx, cond)
        generateBranching(ctx, new IFEQ(null), thenIl, elseIl)
    }

  }

  def generateForNode(ctx: MethodContext, node: sem.Expression, returnUnit: Boolean = true): Unit = {
    import sem._

    def trivialBinOp(arg1: Expression, arg2: Expression, inst: Instruction): Unit = {
      generateForNode(ctx, arg1)
      generateForNode(ctx, arg2)
      ctx.il.append(inst)
    }

    node match {
      case VarDefinition(id, tpe) =>
        ctx.defineVar(id, tpe)

      case FieldAssignment(field, value, read) =>
        generateForNode(ctx, field.expr)
        generateForNode(ctx, value)
        val retVar = if (read) {
          ctx.il.append(InstructionFactory.createDup(jtg.toJavaType(field.tpe).getSize))
          val id = ctx.generateLocalVar(value.tpe)
          ctx.storeVar(id, value.tpe)
          Some(id)
        } else None
        ctx.il.append(ctx.instF.createPutField(jtg.toJavaType(field.structure).getClassName, field.field.name, jtg.toJavaType(field.tpe)))
        retVar.foreach {
          id =>
            ctx.loadVar(id, value.tpe)
        }

      case VarAssignment(id, value, read) =>
        generateForNode(ctx, value)
        if (read) {
          ctx.il.append(InstructionFactory.createDup(jtg.toJavaType(value.tpe).getSize))
        }
        ctx.storeVar(id, value.tpe)

      case VarRead(id, tpe) =>
        ctx.loadVar(id, tpe)

      case FieldAccess(field, st, stExpr) =>
        generateForNode(ctx, stExpr)
        ctx.il.append(ctx.instF.createGetField(jtg.toJavaType(st).getClassName, field.name, jtg.toJavaType(field.tpe)))

      case Block(code, tpe) =>
        generateBlock(ctx, code)

      case FnCall(sig, args) =>
        args.foreach(generateForNode(ctx, _))
        ctx.il.append(
          ctx.instF.createInvoke(
            ctx.cg.getClassName,
            sig.name,
            jtg.toJavaFnRetType(sig.returnType),
            sig.args.map(arg => jtg.toJavaType(arg.tpe)).toArray,
            Constants.INVOKESTATIC
          )
        )

      case Echo(expr) =>
        val pStream = new ObjectType("java.io.PrintStream")
        ctx.il.append(ctx.instF.createFieldAccess("java.lang.System", "out", pStream,
          Constants.GETSTATIC))
        generateForNode(ctx, expr)
        val jType = jtg.toJavaType(expr.tpe)
        val printType = jType match {
          case obj: ObjectType if obj != Type.STRING =>
            ctx.il.append(ctx.instF.createInvoke(
              Type.STRING.getClassName,
              "valueOf",
              Type.STRING,
              Array(Type.OBJECT),
              Constants.INVOKESTATIC
            ))
            Type.STRING
          case arr: ArrayType =>
            val eTpe = arr.getElementType match {
              case ot: ObjectType => Type.OBJECT
              case at: ArrayType => Type.OBJECT //TODO: already need std library here
              case other => other
            }
            ctx.il.append(ctx.instF.createInvoke(
              "java.util.Arrays",
              "toString",
              Type.STRING,
              Array(new ArrayType(eTpe, 1)),
              Constants.INVOKESTATIC
            ))
            Type.STRING
          case other => other
        }
        ctx.il.append(ctx.instF.createInvoke("java.io.PrintStream", "println", Type.VOID,
          Array(printType),
          Constants.INVOKEVIRTUAL))

      case BOperator(arg1, arg2, op) =>

        op match {
          case Operator.IDIV => trivialBinOp(arg1, arg2, new IDIV)
          case Operator.IMUL => trivialBinOp(arg1, arg2, new IMUL)
          case Operator.ISUB => trivialBinOp(arg1, arg2, new ISUB)
          case Operator.IADD => trivialBinOp(arg1, arg2, new IADD)

          case Operator.BAND => trivialBinOp(arg1, arg2, new IAND)
          case Operator.BOR => trivialBinOp(arg1, arg2, new IOR)
          case Operator.BXOR => trivialBinOp(arg1, arg2, new IXOR)

          case Operator.BAND_LZY =>
            generateForNode(ctx, arg1)

            val if1 = ctx.il.append(new IFEQ(null))
            generateForNode(ctx, arg2)
            val if2 = ctx.il.append(new IFEQ(null))
            ctx.il.append(new PUSH(ctx.cpg, 1))
            val gotoEnd = ctx.il.append(new GOTO(null))
            val onFalse = ctx.il.append(new PUSH(ctx.cpg, 0))
            val end = ctx.il.append(new NOP)
            if1.setTarget(onFalse)
            if2.setTarget(onFalse)
            gotoEnd.setTarget(end)

          case Operator.BOR_LZY =>
            generateForNode(ctx, arg1)

            val if1 = ctx.il.append(new IFNE(null))
            generateForNode(ctx, arg2)
            val if2 = ctx.il.append(new IFNE(null))
            ctx.il.append(ctx.PUSH(0))
            val gotoEnd = ctx.il.append(new GOTO(null))
            val onTrue = ctx.il.append(ctx.PUSH(1))
            val end = ctx.il.append(new NOP)
            if1.setTarget(onTrue)
            if2.setTarget(onTrue)
            gotoEnd.setTarget(end)

          case Operator.ILT => generateBoolExpr(ctx, arg1, arg2, new IF_ICMPLT(null))
          case Operator.ILE => generateBoolExpr(ctx, arg1, arg2, new IF_ICMPLE(null))
          case Operator.IGT => generateBoolExpr(ctx, arg1, arg2, new IF_ICMPGT(null))
          case Operator.IGE => generateBoolExpr(ctx, arg1, arg2, new IF_ICMPGE(null))
          case Operator.IEQ => generateBoolExpr(ctx, arg1, arg2, new IF_ICMPEQ(null))
          case Operator.INE => generateBoolExpr(ctx, arg1, arg2, new IF_ICMPNE(null))

          case Operator.CONCAT =>
            generateForNode(ctx, arg1)
            generateForNode(ctx, arg2)
            ctx.il.append(ctx.instF.createInvoke(
              Type.STRING.getClassName,
              "concat",
              Type.STRING,
              Array(Type.STRING),
              Constants.INVOKEVIRTUAL
            ))
        }

      case UOperator(arg, op) =>
        generateForNode(ctx, arg)
        val inst = op match {
          case Operator.INEG => new INEG
          case Operator.BNEG =>
            ctx.il.append(new PUSH(ctx.cpg, 1))
            new IXOR
        }
        ctx.il.append(inst)

      case const: Const =>
        val inst = const match {
          case IntConst(value) =>
            new PUSH(ctx.cpg, value)
          case BoolConst(value) =>
            new PUSH(ctx.cpg, value)
          case StringConst(value) =>
            new PUSH(ctx.cpg, value)
        }
        ctx.il.append(inst)

      case IfExpr(cond, thenBlock, elseBlock, tpe) =>
        val thenIl = new InstructionList()
        val elseIl = new InstructionList()
        generateForNode(ctx.withIL(thenIl), thenBlock, returnUnit)
        generateForNode(ctx.withIL(elseIl), elseBlock, returnUnit)
        val thenIlOpt = if (thenIl.isEmpty) None else Some(thenIl)
        val elseIlOpt = if (elseIl.isEmpty) None else Some(elseIl)
        generateIf(ctx, cond, thenIlOpt, elseIlOpt)

      case Nop =>
        ctx.il.append(new NOP)

      case Consume(expr) =>
        generateForNode(ctx, expr)
        ctx.il.append(InstructionFactory.createPop(jtg.toJavaType(expr.tpe).getSize))

      case si@StructureInstantiation(struct, args, evalOrder) =>
        val sType = jtg.toObjectType(si.tpe)
        ctx.il.append(ctx.instF.createNew(sType))
        ctx.il.append(new DUP)

        val localVars = args.map(e => ctx.generateLocalVar(e.tpe) -> e.tpe).toIndexedSeq
        evalOrder.foreach {
          i =>
            val e = args(i)
            generateForNode(ctx, VarAssignment(localVars(i)._1, e, false), returnUnit = false)
        }
        localVars.foreach {
          case (id, tpe) =>
            ctx.loadVar(id, tpe)
        }
        ctx.il.append(
          ctx.instF.createInvoke(
            sType.getClassName, "<init>",
            Type.VOID,
            args.map(e => jtg.toJavaType(e.tpe)).toArray,
            Constants.INVOKESPECIAL
          )
        )

      case ArrayInstantiation(elemType, args) =>
        val javaElemType = jtg.toJavaType(elemType)
        val size = args.size
        ctx.il.append(new PUSH(ctx.cpg, size))
        ctx.il.append(ctx.instF.createNewArray(javaElemType, 1))
        args.zipWithIndex.foreach {
          case (e, idx) =>
            ctx.il.append(new DUP)
            ctx.il.append(new PUSH(ctx.cpg, idx))
            generateForNode(ctx, e)
            ctx.il.append(InstructionFactory.createArrayStore(javaElemType))
        }

      case ArrayLength(arr) =>
        generateForNode(ctx, arr)
        ctx.il.append(new ARRAYLENGTH)

      case ArrayGet(arr, index, tpe) =>
        generateForNode(ctx, arr)
        generateForNode(ctx, index)
        ctx.il.append(InstructionFactory.createArrayLoad(jtg.toJavaType(tpe)))

      case ArraySet(arr, index, value) =>
        generateForNode(ctx, arr)
        generateForNode(ctx, index)
        generateForNode(ctx, value)
        ctx.il.append(InstructionFactory.createArrayStore(jtg.toJavaType(value.tpe)))

      case ForLoop(itVarId, iterable, body) =>
        generateForNode(ctx, iterable)

        iterable.tpe match {
          case arrTpe@Tpe.Arr(elemType) =>
            val idxVar = ctx.generateLocalVar(Tpe.INT)
            val arrVar = ctx.generateLocalVar(arrTpe)
            ctx.storeVar(arrVar, arrTpe)
            ctx.il.append(new PUSH(ctx.cpg, 0))
            ctx.storeVar(idxVar, Tpe.INT)
            val loopStart = ctx.loadVar(idxVar, Tpe.INT)
            ctx.loadVar(arrVar, arrTpe)
            ctx.il.append(new ARRAYLENGTH)
            val onFail = ctx.il.append(new IF_ICMPGE(null))
            ctx.loadVar(arrVar, arrTpe)
            ctx.loadVar(idxVar, Tpe.INT)
            ctx.il.append(InstructionFactory.createArrayLoad(jtg.toJavaType(elemType)))
            ctx.defineVar(itVarId, elemType)
            ctx.storeVar(itVarId, elemType)
            generateBlock(ctx, body)
            ctx.incrementVar(idxVar, 1)
            ctx.il.append(new GOTO(loopStart))
            val end = ctx.il.append(new NOP)
            onFail.setTarget(end)
          case unknown => throw new RuntimeException("For loop not implemented for " + unknown)
        }
    }
    if (returnUnit && !node.valRet) {
      ctx.il.append(InstructionConstants.ACONST_NULL)
    }
  }

  def generateBlock(ctx: MethodContext, code: Seq[sem.Expression]): Unit = {
    if (code.nonEmpty) {
      code.foreach(generateForNode(ctx, _, returnUnit = false))
    }
  }

  def generateClass(ast: Seq[sem.FnDefinition]): JavaClass = {
    val cg = new ClassGen(className.name, "java.lang.Object", "<generated>", Constants.ACC_PUBLIC | Constants.ACC_SUPER, null)
    val cp = cg.getConstantPool

    val instFactory = new InstructionFactory(cg)

    def generateMethod(code: Seq[sem.Expression], name: String, args: Seq[(String, Type)], retType: Type): Unit = {
      val il = new InstructionList()

      val mg = new MethodGen(
        Constants.ACC_STATIC | Constants.ACC_PUBLIC,
        retType,
        args.map(_._2).toArray,
        args.map(_._1).toArray,
        name,
        className.name,
        il,
        cp
      )
      generateBlock(MethodContext(cg, cp, instFactory, mg, il), code)
      il.append(InstructionFactory.createReturn(retType))
      mg.setMaxStack()
      cg.addMethod(mg.getMethod)
      il.dispose()
    }

    val ics = structs.map(s => InnerClassHelper.makeRecord(className.name, s.name, cp))
    cg.addAttribute(InnerClassHelper.makeAttr(ics, cp))

    ast.foreach {
      case sem.FnDefinition(sig, code) =>
        generateMethod(code, sig.name, sig.args.map(arg => (arg.name, jtg.toJavaType(arg.tpe))), jtg.toJavaFnRetType(sig.returnType))
    }

    cg.getJavaClass
  }

  abstract class BranchInliner {
    def inline(ctx: MethodContext, onTrue: Option[InstructionList], onFalse: Option[InstructionList])
  }

  object BranchInliner {
    def unapply(e: sem.Expression): Option[BranchInliner] = {
      e match {
        case sem.BOperator(e1, e2, op) =>
          val instOp = op match {
            case Operator.ILT => Some(new IF_ICMPLT(null))
            case Operator.ILE => Some(new IF_ICMPLE(null))
            case Operator.IGT => Some(new IF_ICMPGT(null))
            case Operator.IGE => Some(new IF_ICMPGE(null))
            case Operator.IEQ => Some(new IF_ICMPEQ(null))
            case Operator.INE => Some(new IF_ICMPNE(null))
            case _ => None
          }
          instOp.map {
            inst =>
              new BranchInliner {
                override def inline(ctx: MethodContext, onTrue: Option[InstructionList], onFalse: Option[InstructionList]): Unit = {
                  generateForNode(ctx, e1)
                  generateForNode(ctx, e2)
                  generateBranching(ctx, inst, onFalse, onTrue)
                }
              }
          }
        case _ => None
      }
    }
  }

}

