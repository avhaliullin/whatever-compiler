package ru.avhaliullin.whatever.backend

/**
  * @author avhaliullin
  */
case class AccessModifiers(public: Boolean, static: Boolean, fnl: Boolean = false) {

  import org.apache.bcel.Constants._

  def flags: Short = {
    (if (public) ACC_PUBLIC else ACC_PRIVATE) |
      (if (static) ACC_STATIC else 0) |
      (if (fnl) ACC_FINAL else 0)
  }.toShort
}
