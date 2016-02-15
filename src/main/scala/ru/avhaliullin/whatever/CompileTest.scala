package ru.avhaliullin.whatever

import java.io.{File, FilenameFilter}

import ru.avhaliullin.whatever.semantic.module.ClassConverter
import ru.avhaliullin.whatever.semantic.tpe.TypesStore
import ru.avhaliullin.whatever.syntax.{Parser, SyntaxTreeNode}
import ru.avhaliullin.whatever.common.{PrettyPrint, ClassName}
import ru.avhaliullin.whatever.bytecode.{StructureGenerator, ClassBytecodeGenerator}

import scala.io.Source

/**
  * @author avhaliullin
  */
object CompileTest {

  def main(args: Array[String]): Unit = {
    val examplesDir = new File("examples")
    val srcDir = new File(examplesDir, "src")
    val targetDir = new File(examplesDir, "target")
    if (!targetDir.exists()) {
      targetDir.mkdir()
    }
    srcDir.list(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".we")
    }).foreach {
      srcName =>
        println("File: " + srcName)
        val className = srcName.dropRight(".we".length)
        val p = new Parser

        val pr = p.parse(Source.fromFile(new File(srcDir, srcName)).bufferedReader())

        pr match {
          case p.Success(ast: List[SyntaxTreeNode.Definition], _) =>
            println(ast)
            val cc = new ClassConverter
            val (typed, sts) = cc.convert(ast)

            val pp = PrettyPrint.Printer()
            typed.foreach(x => println(pp.print(x.pretty)))

            val cn = ClassName(className)
            val tbg = new ClassBytecodeGenerator(cn, sts)
            val clazz = tbg.generateClass(typed)
            val sg = new StructureGenerator(cn, new TypesStore(sts.map(st => st.name -> st).toMap))

            sts.foreach {
              st =>
                val stClass = sg.generateStruct(st)
                stClass.dump(stClass.getClassName + ".class")
            }
            clazz.dump(new File(targetDir, className + ".class"))

          case p.Failure(msg, pos) =>
            println("Error: " + msg + "\nAt " + pos.offset)
          case p.Error(msg, pos) =>
            println("Error: " + msg + "\nAt " + pos.offset)
        }
    }
  }
}
