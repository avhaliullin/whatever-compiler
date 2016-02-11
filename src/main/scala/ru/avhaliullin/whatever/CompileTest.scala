package ru.avhaliullin.whatever

import java.io.{File, FilenameFilter}

import ru.avhaliullin.whatever.syntax.{Parser, SyntaxTreeNode}
import ru.avhaliullin.whatever.common.ClassName
import ru.avhaliullin.whatever.bytecode.{StructureGenerator, TypedBytecodeGenerator}
import ru.avhaliullin.whatever.semantic.{TypesStore, ClassConverter}

import scala.io.Source

/**
  * @author avhaliullin
  */
object CompileTest {
  def main(args: Array[String]): Unit = {
    val dir = new File(".")
    dir.list(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".we")
    }).foreach {
      srcName =>
        println("File: " + srcName)
        val className = srcName.dropRight(".we".length)
        val p = new Parser

        val pr = p.parse(Source.fromFile(srcName).bufferedReader())

        pr match {
          case p.Success(ast: List[SyntaxTreeNode], _) =>
            println(ast)
            //            val typed = ASTTypeChecker.convert(ast)
            val cc = new ClassConverter
            val (typed, sts) = cc.convert(ast)
            println(typed)
            val cn = ClassName(className)
            val tbg = new TypedBytecodeGenerator(cn, sts)
            val clazz = tbg.generateClass(typed)
            val sg = new StructureGenerator(cn, new TypesStore(sts.map(st => st.name -> st).toMap))

            sts.foreach {
              st =>
                val stClass = sg.generateStruct(st)
                stClass.dump(stClass.getClassName + ".class")
            }
            clazz.dump(className + ".class")

          case p.Failure(msg, pos) =>
            println("Error: " + msg + "\nAt " + pos.offset)
          case p.Error(msg, pos) =>
            println("Error: " + msg + "\nAt " + pos.offset)
        }
    }
  }
}
