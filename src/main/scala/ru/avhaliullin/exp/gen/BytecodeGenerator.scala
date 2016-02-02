//package ru.avhaliullin.exp.gen
//
//import org.apache.bcel.Constants
//import org.apache.bcel.Constants._
//import org.apache.bcel.classfile.JavaClass
//import org.apache.bcel.generic.{IADD, IDIV, IMUL, ISUB, PUSH, _}
//import ru.avhaliullin.exp.ast.ASTNode
//import ru.avhaliullin.exp.ast.ASTNode._
//import ru.avhaliullin.exp.common.ClassName
//
///**
//  * @author avhaliullin
//  */
//object BytecodeGenerator {
//
//  private case class LocalVar(lvg: LocalVariableGen, arg: Boolean) {
//    var assigned = false
//  }
//
//  private case class Context(
//                              cg: ClassGen,
//                              cpg: ConstantPoolGen,
//                              instF: InstructionFactory,
//                              mg: MethodGen,
//                              il: InstructionList,
//                              outerVariables: Map[String, LocalVar],
//                              fnDefs: Map[String, List[FnDefinition.Signature]]
//                            ) {
//
//    var blockVariables: Map[String, LocalVar] = Map()
//
//    def inMethod(sig: FnDefinition.Signature, content: Context => Unit): Unit = {
//      val il = new InstructionList()
//      val retType = getType(sig.returnT, true)
//      val mg = new MethodGen(
//        ACC_STATIC | ACC_PUBLIC,
//        retType,
//        sig.args.map(arg => getType(arg.tpe, false)).toArray,
//        sig.args.map(_.name).toArray,
//        sig.name,
//        cg.getClassName,
//        il,
//        cpg
//      )
//      val ctx = copy(il = il, mg = mg, outerVariables = outerVariables ++ blockVariables)
//      ctx.blockVariables = mg.getLocalVariables.map(lv => lv.getName -> LocalVar(lv, true)).toMap
//
//      content(ctx)
//      il.append(InstructionFactory.createReturn(retType))
//
//      endBlock()
//      mg.setMaxStack()
//      ctx.cg.addMethod(mg.getMethod)
//      il.dispose()
//    }
//
//    def inBlock[T](content: Context => T): T = {
//      val newContext = copy(outerVariables = outerVariables ++ blockVariables)
//      val res = content(newContext)
//      endBlock()
//      res
//    }
//
//    def defineVariable(name: String): Unit = {
//      if (blockVariables.contains(name)) {
//        throw new RuntimeException(s"Variable $name already defined in scope")
//      }
//      val v = mg.addLocalVariable(name, Type.INT, null, null)
//      blockVariables += (name -> LocalVar(v, false))
//    }
//
//    private def getVariable(name: String): LocalVar = {
//      blockVariables.getOrElse(name, outerVariables.getOrElse(name, throw new RuntimeException(s"Variable $name not found")))
//    }
//
//    def assignVariable(name: String, initializer: Int => Instruction): InstructionHandle = {
//      val v = getVariable(name)
//      val inst = initializer(v.lvg.getIndex)
//      val ih = il.append(inst)
//      if (!v.arg && !v.assigned) {
//        v.lvg.setStart(ih)
//        v.assigned = true
//      }
//      ih
//    }
//
//    def assignVariable(name: String): InstructionHandle = {
//      assignVariable(name, new ISTORE(_))
//    }
//
//    def useVariable(name: String): Int = {
//      val v = getVariable(name)
//      if (!v.arg && !v.assigned) {
//        throw new RuntimeException(s"Variable $name is not assigned yet")
//      }
//      v.lvg.getIndex
//    }
//
//    def endBlock(): Unit = {
//      blockVariables.values.foreach { lv =>
//        if (lv.lvg.getEnd == null) {
//          lv.lvg.setEnd(il.getEnd)
//        }
//      }
//    }
//  }
//
//  private def generate(ctx: Context, ast: ASTNode): Unit = {
//    ast match {
//      case Assignment(name, exp) =>
//        generate(ctx, exp)
//        ctx.assignVariable(name)
//
//      case c: IntConst =>
//        ctx.il.append(new PUSH(ctx.cpg, c.value))
//
//      case BinaryOperator(l, r, op) =>
//        generate(ctx, l)
//        generate(ctx, r)
//        val instruction = op match {
//          case "+" => new IADD()
//          case "-" => new ISUB()
//          case "*" => new IMUL()
//          case "/" => new IDIV()
//        }
//        ctx.il.append(instruction)
//
//      case Echo(e) =>
//        val pStream = new ObjectType("java.io.PrintStream")
//        ctx.il.append(ctx.instF.createFieldAccess("java.lang.System", "out", pStream,
//          Constants.GETSTATIC))
//        generate(ctx, e)
//        ctx.il.append(ctx.instF.createInvoke("java.io.PrintStream", "println", Type.VOID,
//          Array(Type.INT),
//          Constants.INVOKEVIRTUAL))
//
//      case Block(sts) =>
//        ctx.inBlock {
//          newCtx =>
//            sts.foreach(generate(newCtx, _))
//        }
//
//      case VarDefinition(name) =>
//        ctx.defineVariable(name)
//
//      case Variable(name) =>
//        val idx = ctx.useVariable(name)
//        ctx.il.append(new ILOAD(idx))
//
//      case FnDefinition(sig, code) =>
//        ctx.inMethod(sig, {
//          ctx =>
//            code.foreach(generate(ctx, _))
//        })
//
//      case FnCall(name, args) =>
//        args.foreach(generate(ctx, _))
//
//        // Заглушка, пока только int
//        val sig = ctx.fnDefs.getOrElse(name, List()).find(_.args.size == args.size)
//          .getOrElse(throw new RuntimeException(s"Not found method $name with ${args.size} arguments"))
//
//        ctx.il.append(
//          ctx.instF.createInvoke(
//            ctx.cg.getClassName,
//            name,
//            getType(sig.returnT, true),
//            sig.args.map(arg => getType(arg.tpe, false)).toArray,
//            Constants.INVOKESTATIC
//          )
//        )
//    }
//  }
//
//  def getType(name: String, allowVoid: Boolean): Type = {
//    name match {
//      case "int" => Type.INT
//      case "void" if allowVoid => Type.VOID
//      case _ => throw new RuntimeException(s"Unknown type: $name")
//    }
//  }
//
//  private def javaSignature(fnDefinition: FnDefinition.Signature): String = {
//
//    Type.getMethodSignature(getType(fnDefinition.returnT, true), fnDefinition.args.map(arg => getType(arg.tpe, false)).toArray)
//  }
//
//  private def assertFnSignaturesUnique(sigs: Map[String, List[FnDefinition.Signature]]): Unit = {
//    sigs.values.filter(_.size > 1).foreach {
//      overloaded =>
//        overloaded.foldLeft(Set[FnDefinition.Signature]()) {
//          (acc, it) =>
//            if (acc(it)) {
//              throw new RuntimeException(s"Duplicated method with signature $it")
//            } else {
//              acc + it
//            }
//        }
//    }
//  }
//
//  def generateClass(name: ClassName, ast: List[ASTNode]): JavaClass = {
//    val cg = new ClassGen(name.name, "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, null)
//    val cp = cg.getConstantPool
//    val mainIl = new InstructionList()
//
//    val mainMg = new MethodGen(
//      ACC_STATIC | ACC_PUBLIC,
//      Type.VOID,
//      Array(new ArrayType(Type.STRING, 1)),
//      Array("argv"),
//      "main",
//      name.name,
//      mainIl,
//      cp
//    )
//
//    val instFactory = new InstructionFactory(cg)
//    val fnDefs = ast.collect {
//      case FnDefinition(sig, _) =>
//        sig
//    }.groupBy(_.name)
//
//    assertFnSignaturesUnique(fnDefs)
//
//    val ctx = new Context(cg, cp, instFactory, mainMg, mainIl, Map(), fnDefs)
//
//    val (methodsAst, mainAst) = ast.partition(_.isDefinition)
//
//    val methods = methodsAst.map {
//      case definition: Definition => definition
//      case _ => throw new RuntimeException("Should never happen")
//    }
//
//    val main = mainAst.map {
//      case st: Statement => st
//      case _ => throw new RuntimeException("Should never happen")
//    }
//
//    methods.foreach(generate(ctx, _))
//    generate(ctx, Block(main))
//
//    mainIl.append(InstructionConstants.RETURN)
//
//    mainMg.setMaxStack()
//    cg.addMethod(mainMg.getMethod)
//    mainIl.dispose() // Allow instruction handles to be reused
//    cg.getJavaClass
//  }
//}
