package ru.avhaliullin.whatever.semantic

/**
  * @author avhaliullin
  */
sealed trait BlockId

object BlockId {

  case object MethodArg extends BlockId

  case class LocalVar(id: Int) extends BlockId

  class Gen {
    private var num: Int = 0

    def next: BlockId = {
      num = num + 1
      LocalVar(num)
    }
  }

}