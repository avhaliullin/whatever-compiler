package ru.avhaliullin.exp.gen

import org.apache.bcel.Constants
import org.apache.bcel.Constants._
import org.apache.bcel.classfile.JavaClass
import org.apache.bcel.generic._
import ru.avhaliullin.exp.common.ClassName
import ru.avhaliullin.exp.typed._

/**
  * @author avhaliullin
  */
object TypedBytecodeGenerator {

  private def toJavaType(tpe: Tpe): Type = {
    tpe match {
      case Tpe.BOOL => Type.BOOLEAN
      case Tpe.INT => Type.INT
      case Tpe.UNIT => Type.VOID
    }
  }

  private case class MethodContext(
                                    cg: ClassGen,
                                    cpg: ConstantPoolGen,
                                    instF: InstructionFactory,
                                    mg: MethodGen,
                                    il: InstructionList
                                  ) {
    var vars: Map[VarId, LocalVariableGen] = mg.getLocalVariables.map {
      lvg =>
        VarId(lvg.getName, BlockId.MethodArg) -> lvg
    }.toMap

    def defineVar(id: VarId, tpe: Tpe): Unit = {
      vars += id -> mg.addLocalVariable(id.name, toJavaType(tpe), null, null)
    }

    def storeVar(id: VarId, tpe: Tpe): InstructionHandle = {
      assignVar(id, InstructionFactory.createStore(toJavaType(tpe), _))
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
      val ih = il.append(InstructionFactory.createLoad(toJavaType(tpe), lvg.getIndex))
      lvg.setEnd(ih)
      ih
    }

    def withIL(il: InstructionList): MethodContext = {
      val res = copy(il = il)
      res.vars = vars
      res
    }

    def PUSH(i: Int) = new PUSH(cpg, i)
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

  def generateBoolExpr(ctx: MethodContext, arg1: TypedASTNode.Expression, arg2: TypedASTNode.Expression, bInst: BranchInstruction): Unit = {
    generateForNode(ctx, arg1)
    generateForNode(ctx, arg2)
    generateBranching(ctx, bInst, Some(new InstructionList(ctx.PUSH(0))), Some(new InstructionList(ctx.PUSH(1))))
  }

  def generateIf(ctx: MethodContext, cond: TypedASTNode.Expression, thenIl: Option[InstructionList], elseIl: Option[InstructionList]): Unit = {
    cond match {
      case BranchInliner(inliner) =>
        inliner.inline(ctx, thenIl, elseIl)

      case _ =>
        generateForNode(ctx, cond)
        generateBranching(ctx, new IFEQ(null), thenIl, elseIl)
    }

  }

  def generateForNode(ctx: MethodContext, node: TypedASTNode.Expression): Unit = {
    import TypedASTNode._

    def trivialBinOp(arg1: Expression, arg2: Expression, inst: Instruction): Unit = {
      generateForNode(ctx, arg1)
      generateForNode(ctx, arg2)
      ctx.il.append(inst)
    }

    node match {
      case VarDefinition(id, tpe) =>
        ctx.defineVar(id, tpe)

      case VarAssignment(id, expr) =>
        generateForNode(ctx, expr)
        ctx.storeVar(id, expr.tpe)

      case VarRead(id, tpe) =>
        ctx.loadVar(id, tpe)

      case Block(code, tpe) =>
        code.foreach(generateForNode(ctx, _))

      case FnCall(sig, args) =>
        args.foreach(generateForNode(ctx, _))
        ctx.il.append(
          ctx.instF.createInvoke(
            ctx.cg.getClassName,
            sig.name,
            toJavaType(sig.returnType),
            sig.args.map(arg => toJavaType(arg.tpe)).toArray,
            Constants.INVOKESTATIC
          )
        )

      case Echo(expr) =>
        val pStream = new ObjectType("java.io.PrintStream")
        ctx.il.append(ctx.instF.createFieldAccess("java.lang.System", "out", pStream,
          Constants.GETSTATIC))
        generateForNode(ctx, expr)
        ctx.il.append(ctx.instF.createInvoke("java.io.PrintStream", "println", Type.VOID,
          Array(toJavaType(expr.tpe)),
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
        }
        ctx.il.append(inst)

      case IfExpr(cond, thenBlock, elseBlock, tpe) =>
        val thenIl = new InstructionList()
        val elseIl = new InstructionList()
        generateForNode(ctx.withIL(thenIl), thenBlock)
        generateForNode(ctx.withIL(elseIl), elseBlock)
        val thenIlOpt = if (thenIl.isEmpty) None else Some(thenIl)
        val elseIlOpt = if (elseIl.isEmpty) None else Some(elseIl)
        generateIf(ctx, cond, thenIlOpt, elseIlOpt)

      case Nop =>
        ctx.il.append(new NOP)

      case Pop(tpe) =>
        ctx.il.append(InstructionFactory.createPop(toJavaType(tpe).getSize))
    }
  }

  def generateBlock(ctx: MethodContext, code: Seq[TypedASTNode.Expression]): Unit = {
    code.foreach(generateForNode(ctx, _))
  }

  def generateClass(className: ClassName, ast: Seq[TypedASTNode.Definition]): JavaClass = {
    val cg = new ClassGen(className.name, "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, null)
    val cp = cg.getConstantPool

    val instFactory = new InstructionFactory(cg)

    def generateMethod(code: Seq[TypedASTNode.Expression], name: String, args: Seq[(String, Type)], retType: Type): Unit = {
      val il = new InstructionList()

      val mg = new MethodGen(
        ACC_STATIC | ACC_PUBLIC,
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

    ast.foreach {
      case TypedASTNode.FnDefinition(sig, code) =>
        generateMethod(code, sig.name, sig.args.map(arg => (arg.name, toJavaType(arg.tpe))), toJavaType(sig.returnType))
      case TypedASTNode.Main(code) =>
        generateMethod(code, "main", Seq(("args", new ArrayType(Type.STRING, 1))), Type.VOID)
    }

    cg.getJavaClass
  }

  abstract class BranchInliner {
    def inline(ctx: MethodContext, onTrue: Option[InstructionList], onFalse: Option[InstructionList])
  }

  object BranchInliner {
    def unapply(e: TypedASTNode.Expression): Option[BranchInliner] = {
      e match {
        case TypedASTNode.BOperator(e1, e2, op) =>
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

