package ru.avhaliullin.whatever.frontend

import java.io.File

import ru.avhaliullin.whatever.common.CompilationException
import ru.avhaliullin.whatever.frontend.sources.{SourceTree, SourceTreeNode}
import ru.avhaliullin.whatever.frontend.syntax.{Parser, SyntaxTree, SyntaxTreeNode}

import scala.io.Source

/**
  * @author avhaliullin
  */
object Frontend {
  def parseSources(sources: SourceTree[Unit]): SourceTree[SyntaxTree] = {
    val p = new Parser
    sources.map {
      srcFile =>
        val pr = p.parse(Source.fromFile(srcFile.file).bufferedReader())
        pr match {
          case p.Success(tree: SyntaxTree, _) => tree
          case p.Failure(msg, pos) =>
            throw new CompilationException(srcFile.file, pos.pos, "Syntax error: " + msg)
          case p.Error(msg, pos) =>
            throw new CompilationException(srcFile.file, pos.pos, "Syntax error: " + msg)
        }
    }
  }

  def parseSources(sourcesRoot: File): SourceTree[SyntaxTree] = {
    parseSources(SourceTreeNode.findSources(sourcesRoot))
  }
}
