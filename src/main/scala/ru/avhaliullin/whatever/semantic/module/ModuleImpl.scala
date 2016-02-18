package ru.avhaliullin.whatever.semantic.module

import ru.avhaliullin.whatever.semantic.{SemanticTreeNode, Structure}

/**
  * @author avhaliullin
  */
case class ModuleImpl(name: ModuleName, structs: Seq[Structure], fns: Seq[SemanticTreeNode.FnDefinition])
