package ru.avhaliullin.whatever.semantic

import ru.avhaliullin.whatever.frontend.syntax.{Import, QualifiedName}
import ru.avhaliullin.whatever.semantic.module.ModuleName

/**
  * @author avhaliullin
  */
case class ImportsContext(module: ModuleName, importedNames: Map[String, ModuleName]) {
  def resolveName(qn: QualifiedName): (String, ModuleName) = {
    qn match {
      case QualifiedName.Absolute(parts) => (parts.last, parts.dropRight(1).foldLeft(ModuleName.ROOT: ModuleName)(_ sub _))
      case QualifiedName.Relative(parts) =>
        importedNames.get(parts.head) match {
          case Some(srcModule) => (parts.last, parts.dropRight(1).foldLeft(srcModule)(_ sub _))
          case None => (parts.last, parts.dropRight(1).foldLeft(module)(_ sub _))
        }
    }
  }

  def withImports(imports: Seq[Import]): ImportsContext = {
    val newImports = imports.foldLeft(importedNames) {
      (acc, it) =>
        val (name, moduleName) = it.name match {
          case QualifiedName.Absolute(parts) => (parts.last, parts.dropRight(1).foldLeft(ModuleName.ROOT: ModuleName)(_ sub _))
          case QualifiedName.Relative(parts) => (parts.last, parts.dropRight(1).foldLeft(module)(_ sub _))
        }
        if (acc.contains(name)) {
          throw new RuntimeException(s"Imports conflict: name $name imported from ${acc(name)} and $moduleName")
        }
        acc + (name -> moduleName)
    }
    copy(importedNames = newImports)
  }
}

object ImportsContext {
  def defaultContext(module: ModuleName): ImportsContext =
    ImportsContext(
      module,
      Map(
        "Int" -> ModuleName.Default.std,
        "Boolean" -> ModuleName.Default.std,
        "Array" -> ModuleName.Default.std,
        "Unit" -> ModuleName.Default.std,
        "String" -> ModuleName.Default.std
      )
    )

  def fromImports(module: ModuleName, imports: Seq[Import]): ImportsContext = {
    defaultContext(module).withImports(imports)
  }
}