package ru.avhaliullin.exp.gen

import org.apache.bcel.Constants
import org.apache.bcel.Constants._
import org.apache.bcel.classfile.JavaClass
import org.apache.bcel.generic.{IADD, IDIV, IMUL, ISUB, PUSH, _}
import ru.avhaliullin.exp.ast.ASTNode
import ru.avhaliullin.exp.ast.ASTNode._
import ru.avhaliullin.exp.common.ClassName

/**
  * @author avhaliullin
  */
object BytecodeGenerator {

  private case class Context(
                              cg: ClassGen,
                              cpg: ConstantPoolGen,
                              instF: InstructionFactory,
                              mg: MethodGen,
                              il: InstructionList,
                              outerVariables: Map[String, LocalVariableGen],
                              fnDefs: Map[String, List[FnDefinition.Signature]]
                            ) {
    var blockVariables: Map[String, LocalVariableGen] = Map()

    def inBlock[T](content: Context => T): T = {
      val newContext = copy(outerVariables = outerVariables ++ blockVariables)
      val res = content(newContext)
      endBlock()
      res
    }

    def defineVariable(name: String): LocalVariableGen = {
      if (blockVariables.contains(name)) {
        throw new RuntimeException(s"Variable $name already defined in scope")
      }
      val v = mg.addLocalVariable(name, Type.INT, null, null)
      blockVariables += (name -> v)
      v
    }

    private def getVariable(name: String): LocalVariableGen = {
      blockVariables.getOrElse(name, outerVariables.getOrElse(name, throw new RuntimeException(s"Variable $name not found")))
    }

    def assignVariable(name: String): InstructionHandle = {
      val v = getVariable(name)
      val inst = new ISTORE(v.getIndex)
      val ih = il.append(inst)
      if (v.getStart == null) {
        v.setStart(ih)
      }
      ih
    }

    def useVariable(name: String): Int = {
      val v = getVariable(name)
      if (v.getStart == null) {
        throw new RuntimeException(s"Variable $name is not assigned yet")
      }
      v.getIndex
    }

    def endBlock(): Unit = {
      val lastHandle = il.getEnd
      blockVariables.values.foreach(_.setEnd(lastHandle))
    }
  }

  private def generate(ctx: Context, ast: ASTNode): Unit = {
    ast match {
      case Assignment(name, exp) =>
        generate(ctx, exp)
        ctx.assignVariable(name)

      case c: Const =>
        ctx.il.append(new PUSH(ctx.cpg, c.value))

      case Operator(l, r, op) =>
        generate(ctx, l)
        generate(ctx, r)
        val instruction = op match {
          case "+" => new IADD()
          case "-" => new ISUB()
          case "*" => new IMUL()
          case "/" => new IDIV()
        }
        ctx.il.append(instruction)

      case Echo(e) =>
        val pStream = new ObjectType("java.io.PrintStream")
        ctx.il.append(ctx.instF.createFieldAccess("java.lang.System", "out", pStream,
          Constants.GETSTATIC))
        generate(ctx, e)
        ctx.il.append(ctx.instF.createInvoke("java.io.PrintStream", "println", Type.VOID,
          Array(Type.INT),
          Constants.INVOKEVIRTUAL))

      case Block(sts) =>
        ctx.inBlock {
          newCtx =>
            sts.foreach(generate(newCtx, _))
        }

      case VarDefinition(name) =>
        ctx.defineVariable(name)

      case Variable(name) =>
        val idx = ctx.useVariable(name)
        ctx.il.append(new ILOAD(idx))
    }
  }

  def getType(name: String, allowVoid: Boolean): Type = {
    name match {
      case "int" => Type.INT
      case "void" if allowVoid => Type.VOID
      case _ => throw new RuntimeException(s"Unknown type: $name")
    }
  }

  private def javaSignature(fnDefinition: FnDefinition.Signature): String = {

    Type.getMethodSignature(getType(fnDefinition.returnT, true), fnDefinition.args.map(arg => getType(arg.tpe, false)).toArray)
  }

  private def assertFnSignaturesUnique(sigs: Map[String, List[FnDefinition.Signature]]): Unit = {
    sigs.values.filter(_.size > 1).foreach {
      overloaded =>
        overloaded.foldLeft(Set[FnDefinition.Signature]()) {
          (acc, it) =>
            if (acc(it)) {
              throw new RuntimeException(s"Duplicated method with signature $it")
            } else {
              acc + it
            }
        }
    }
  }

  def generateClass(name: ClassName, ast: List[ASTNode]): JavaClass = {
    val cg = new ClassGen(name.name, "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, null)
    val cp = cg.getConstantPool
    val mainIl = new InstructionList()

    val mainMg = new MethodGen(
      ACC_STATIC | ACC_PUBLIC,
      Type.VOID,
      Array(new ArrayType(Type.STRING, 1)),
      Array("argv"),
      "main",
      name.name,
      mainIl,
      cp
    )

    val instFactory = new InstructionFactory(cg)
    val fnDefs = ast.collect {
      case FnDefinition(sig, _) =>
        sig
    }.groupBy(_.name)

    assertFnSignaturesUnique(fnDefs)

    val ctx = new Context(cg, cp, instFactory, mainMg, mainIl, Map(), fnDefs)

    val (methodsAst, mainAst) = ast.partition(_.isDefinition)

    val methods = methodsAst.map {
      case definition: Definition => definition
      case _ => throw new RuntimeException("Should never happen")
    }

    val main = mainAst.map {
      case st: Statement => st
      case _ => throw new RuntimeException("Should never happen")
    }

    methods.foreach {
      case FnDefinition(sig, code) =>
        val il = new InstructionList()
        val mg = new MethodGen(
          ACC_STATIC | ACC_PUBLIC,
          getType(sig.returnT, true),
          sig.args.map(arg => getType(arg.tpe, false)).toArray,
          sig.args.map(_.name).toArray,
          sig.name,
          name.name,
          il,
          cp
        )
        val methodCtx = ctx.copy(il = il, mg = mg)
        generate(methodCtx, Block(code))
        mg.setMaxStack()
        il.dispose()
        cg.addMethod(mg.getMethod)
    }

    generate(ctx, Block(main))
    mainIl.append(InstructionConstants.RETURN)

    mainMg.setMaxStack()
    cg.addMethod(mainMg.getMethod)
    mainIl.dispose() // Allow instruction handles to be reused
    cg.addEmptyConstructor(ACC_PUBLIC)
    cg.getJavaClass
  }
}
