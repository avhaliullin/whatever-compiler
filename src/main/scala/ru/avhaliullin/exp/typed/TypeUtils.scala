package ru.avhaliullin.exp.typed

/**
  * @author avhaliullin
  */
object TypeUtils {
  def isAssignable(from: Tpe, to: Tpe): Boolean = {
    to == Tpe.UNIT || to == from
  }

  def assertAssignable(from: Tpe, to: Tpe): Unit = {
    if (!isAssignable(from, to)) {
      throw new RuntimeException(s"Type check failed: expected $to, actual $from")
    }
  }

  def getUpperBoundType(t1: Tpe, t2: Tpe): Tpe = {
    if (t1 == t2) {
      t1
    } else {
      Tpe.UNIT
    }
  }

}
