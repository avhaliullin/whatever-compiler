package ru.avhaliullin.whatever.backend

import org.apache.bcel.Constants
import org.apache.bcel.Constants._
import org.apache.bcel.classfile.JavaClass
import org.apache.bcel.generic._
import ru.avhaliullin.whatever.semantic.tpe.JavaTypeGen
import ru.avhaliullin.whatever.semantic.{SemanticTreeNode => sem, Structure}

/**
  * @author avhaliullin
  */
class StructureGenerator {
  val jtg = JavaTypeGen

  def generateStruct(st: Structure, methods: Seq[sem.FnDefinition]): JavaClass = {
    def loadThis() = InstructionFactory.createLoad(jtg.toJavaType(st), 0)

    val sJType = jtg.toJavaType(st)
    val sClassName = sJType.getClassName

    val codeGen = new ClassBytecodeGenerator(st.fullTpe.module, sClassName)

    val cg = new ClassGen(sClassName, "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, null)
    val cp = cg.getConstantPool
    // Add fields
    val instFactory = new InstructionFactory(cg)
    st.fields.foreach {
      field =>
        val fg = new FieldGen(Constants.ACC_PUBLIC, jtg.toJavaType(field.tpe), field.name, cp)
        cg.addField(fg.getField)
    }

    {
      val il = new InstructionList()

      // Generate constructor
      il.append(loadThis())
      il.append(instFactory.createInvoke("java.lang.Object", "<init>", Type.VOID, Array(), Constants.INVOKESPECIAL))

      val mg = new MethodGen(
        ACC_PUBLIC,
        Type.VOID,
        st.fields.map(f => jtg.toJavaType(f.tpe)).toArray,
        st.fields.map(_.name).toArray,
        "<init>",
        sClassName,
        il,
        cp
      )
      val argName2Arg = mg.getLocalVariables.map(lv => lv.getName -> lv).toMap
      st.fields.foreach {
        f =>
          il.append(loadThis())
          il.append(InstructionFactory.createLoad(jtg.toJavaType(f.tpe), argName2Arg(f.name).getIndex))
          il.append(instFactory.createPutField(sClassName, f.name, jtg.toJavaType(f.tpe)))
      }
      il.append(InstructionConstants.RETURN)
      mg.setMaxStack()
      cg.addMethod(mg.getMethod)
      il.dispose()
    }

    // Generate toString()
    {
      val il = new InstructionList()
      val mg = new MethodGen(
        ACC_PUBLIC,
        Type.STRING,
        Array(),
        Array(),
        "toString",
        sClassName,
        il,
        cp
      )

      val sbClass = "java.lang.StringBuilder"

      il.append(instFactory.createNew(sbClass))
      il.append(new DUP)
      il.append(new PUSH(cp, st.fullTpe.name + "("))
      il.append(
        instFactory.createInvoke(
          sbClass,
          "<init>",
          Type.VOID,
          Array(new ObjectType("java.lang.CharSequence")),
          Constants.INVOKESPECIAL)
      )

      def createToString(): Instruction = {
        instFactory.createInvoke(
          Type.STRING.getClassName,
          "valueOf",
          Type.STRING,
          Array(Type.OBJECT),
          Constants.INVOKESTATIC
        )
      }

      def createAppend(tpe: Type): Instruction = {
        instFactory.createInvoke(
          sbClass,
          "append",
          new ObjectType(sbClass),
          Array(tpe),
          Constants.INVOKEVIRTUAL
        )
      }

      if (st.fields.nonEmpty) {
        def dumpField(f: Structure.Field): Unit = {
          val fType = jtg.toJavaType(f.tpe)
          il.append(loadThis())
          il.append(instFactory.createGetField(sClassName, f.name, fType))
          fType match {
            case obj: ObjectType if obj != Type.STRING =>
              il.append(createToString())
              il.append(createAppend(Type.STRING))
            case _ =>
              il.append(createAppend(fType))
          }
        }

        val head = st.fields.dropRight(1)
        val tail = st.fields.last
        head.foreach {
          f =>
            dumpField(f)
            il.append(new PUSH(cp, ", "))
            il.append(createAppend(Type.STRING))
        }
        dumpField(tail)
      }
      il.append(new PUSH(cp, ")"))
      il.append(createAppend(Type.STRING))

      il.append(createToString())

      il.append(InstructionFactory.createReturn(Type.STRING))
      mg.setMaxStack()
      cg.addMethod(mg.getMethod)
      il.dispose()
    }

    methods.foreach {
      method =>
        codeGen.generateMethod(cg, cp, instFactory, AccessModifiers(public = true, static = false, fnl = true), method)
    }

    cg.getJavaClass
  }
}

object StructureGenerator extends StructureGenerator