package ru.avhaliullin.whatever

import java.io.File

import ru.avhaliullin.whatever.bytecode.{StructureGenerator, ClassBytecodeGenerator}
import ru.avhaliullin.whatever.common.{PrettyPrint, ClassName}
import ru.avhaliullin.whatever.semantic.{ClassConverter, TypesStore}
import ru.avhaliullin.whatever.syntax.{SyntaxTreeNode, Parser}

import scala.io.Source

/**
  * @author avhaliullin
  */
object Compile {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      throw new RuntimeException("No src files was passed")
    }
    args.map {
      arg =>
        val f = new File(arg)
        if (!f.exists()) {
          throw new RuntimeException(s"$arg doesn't exist")
        }
        if (!f.isFile) {
          throw new RuntimeException(s"$arg is not a file")
        }
        if (!f.getName.endsWith(".we")) {
          throw new RuntimeException(s"$arg has unknown file name extension (only .we allowed)")
        }
        f.getAbsoluteFile
    }.foreach {
      file =>
        println("File: " + file)

        val className = file.getName.dropRight(".we".length)
        val p = new Parser

        val pr = p.parse(Source.fromFile(file).bufferedReader())

        pr match {
          case p.Success(ast: List[SyntaxTreeNode], _) =>
            println(ast)
            //            val typed = ASTTypeChecker.convert(ast)
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
            clazz.dump(new File(file.getParentFile, className + ".class"))

          case p.Failure(msg, pos) =>
            println("Error: " + msg + "\nAt " + pos.offset)
          case p.Error(msg, pos) =>
            println("Error: " + msg + "\nAt " + pos.offset)
        }
    }

  }
}
