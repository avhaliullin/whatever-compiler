package ru.avhaliullin.exp.typed

/**
  * @author avhaliullin
  */
class FnStore(fns: Map[String, Set[FnSignature]]) {
  def find(name: String, args: Seq[Tpe]): FnSignature = {
    fns.getOrElse(name, Set()).find {
      sig =>
        sig.args.size == args.size && sig.args.map(_.tpe).zip(args).forall {
          case (expect, actual) => TypeUtils.isAssignable(actual, expect)
        }
    }.getOrElse(throw new RuntimeException(s"Cannot find function $name with such signature"))

  }
}
