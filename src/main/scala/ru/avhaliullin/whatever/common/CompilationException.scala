package ru.avhaliullin.whatever.common

import java.io.File

import scala.util.parsing.input.Position

/**
  * @author avhaliullin
  */
class CompilationException(src: File, pos: Position, message: String) extends RuntimeException(s"${src.getAbsolutePath}[${pos.line}:${pos.column}] $message")
