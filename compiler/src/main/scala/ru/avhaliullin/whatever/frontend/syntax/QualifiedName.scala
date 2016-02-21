package ru.avhaliullin.whatever.frontend.syntax

/**
  * @author avhaliullin
  */
sealed trait QualifiedName {
  def parts: Seq[String]

  def relative: Boolean
}

object QualifiedName {

  case class Relative(parts: Seq[String]) extends QualifiedName {
    val relative = true
  }

  case class Absolute(parts: Seq[String]) extends QualifiedName {
    val relative = false
  }

}
