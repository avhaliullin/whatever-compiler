package ru.avhaliullin.exp.gen

import org.apache.bcel.Constants
import org.apache.bcel.Constants._
import org.apache.bcel.classfile.JavaClass
import org.apache.bcel.generic._
import ru.avhaliullin.exp.common.ClassName
import ru.avhaliullin.exp.typed.{BlockId, Tpe, TypedASTNode, VarId}

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

  }

  def generateForNode(ctx: MethodContext, node: TypedASTNode.Expression): Unit = {
    import TypedASTNode._

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

      case bOp: BinaryOperator =>
        generateForNode(ctx, bOp.arg1)
        generateForNode(ctx, bOp.arg2)
        bOp match {
          case _: OpIDiv => ctx.il.append(new IDIV)
          case _: OpIMul => ctx.il.append(new IMUL)
          case _: OpISub => ctx.il.append(new ISUB)
          case _: OpIAdd => ctx.il.append(new IADD)

          case icmp: ICmp =>
            val ifInst = icmp match {
              case _: OpILt => new IF_ICMPLT(null)
              case _: OpILte => new IF_ICMPLE(null)
              case _: OpIGt => new IF_ICMPGT(null)
              case _: OpIGte => new IF_ICMPGE(null)
              case _: OpIEq => new IF_ICMPEQ(null)
              case _: OpINeq => new IF_ICMPNE(null)
            }
            ctx.il.append(ifInst)
            ctx.il.append(new PUSH(ctx.cpg, 0))
            val jmp = ctx.il.append(new GOTO(null))
            val p0 = ctx.il.append(new PUSH(ctx.cpg, 1))
            val end = ctx.il.append(new NOP)
            ifInst.setTarget(p0)
            jmp.setTarget(end)
        }

      case uOp: UnaryOperator =>
        generateForNode(ctx, uOp.arg)
        val inst = uOp match {
          case _: OpINeg => new INEG
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
}

