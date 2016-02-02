package ru.avhaliullin.exp

import ru.avhaliullin.exp.ast.ASTNode
import ru.avhaliullin.exp.common.ClassName
import ru.avhaliullin.exp.gen.TypedBytecodeGenerator
import ru.avhaliullin.exp.parse.Parser
import ru.avhaliullin.exp.typed.ASTTypeChecker

import scala.io.Source

/**
  * @author avhaliullin
  */
object CompileTest {
  def main(args: Array[String]): Unit = {
    val name = "Expression"
    val p = new Parser

    val pr = p.parse(Source.fromFile(name + ".av").bufferedReader())

    pr match {
      case p.Success(ast: List[ASTNode], _) =>
        println(ast)
        val typed = ASTTypeChecker.convert(ast)
        println(typed)
        val clazz = TypedBytecodeGenerator.generateClass(ClassName(name), typed)
        clazz.dump(name + ".class")

      case p.Failure(msg, pos) =>
        println("Error: " + msg + "\nAt " + pos.offset)
      case p.Error(msg, pos) =>
        println("Error: " + msg + "\nAt " + pos.offset)
    }

  }
}
