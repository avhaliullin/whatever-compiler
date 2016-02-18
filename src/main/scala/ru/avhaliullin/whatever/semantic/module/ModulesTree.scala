package ru.avhaliullin.whatever.semantic.module

/**
  * @author avhaliullin
  */
case class ModulesTree[T](name: ModuleName, submodules: Seq[ModulesTree[T]], payload: T) {
  def map[R](f: ModulesTree[T] => R): ModulesTree[R] = copy(submodules = submodules.map(_ map f), payload = f(this))

  def fold[R](zero: R)(f: (R, ModulesTree[T]) => R): R = submodules.foldLeft(f(zero, this)) {
    (acc, it) =>
      it.fold(acc)(f)
  }
}
