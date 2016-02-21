package ru.avhaliullin.whatever.semantic.module

import ru.avhaliullin.whatever.frontend.sources.{SourceTree, SourceTreeNode}
import ru.avhaliullin.whatever.frontend.syntax.SyntaxTree
import ru.avhaliullin.whatever.frontend.{syntax => syn}
import ru.avhaliullin.whatever.semantic.function.{FnAnalyzer, FnConverter, FnSignature}
import ru.avhaliullin.whatever.semantic.{ImportsContext, SemanticTreeNode => sem, StructAnalyzer, VarIdGen}

/**
  * @author avhaliullin
  */
class ModuleConverter {
  private def mapSourcesToModules(sources: Seq[SourceTreeNode[SyntaxTree]], curModule: ModuleName): ModulesTree[Seq[SyntaxTree]] = {
    val (syntaxTrees, submodules) = sources.foldLeft((Seq[SyntaxTree](), Seq[ModulesTree[Seq[SyntaxTree]]]())) {
      case ((syntaxTreesAcc, submodulesAcc), src) =>
        src match {
          case SourceTreeNode.SrcFile(_, _, ast) =>
            (syntaxTreesAcc :+ ast, submodulesAcc)
          case SourceTreeNode.SrcDirectory(name, children, _) =>
            (syntaxTreesAcc, submodulesAcc :+ mapSourcesToModules(children, curModule.sub(name)))
        }
    }
    ModulesTree(curModule, submodules, syntaxTrees)
  }

  def mapSourcesToModules(src: SourceTree[SyntaxTree]): ModulesTree[Seq[SyntaxTree]] = mapSourcesToModules(src.sources, ModuleName.ROOT)

  def makeModulesAPI(modules: ModulesTree[Seq[SyntaxTree]]): ModulesTree[ModuleAPI] = {
    modules.map {
      module =>
        module.payload.foldLeft(ModuleAPI(module.name, Map(), Map())) {
          case (moduleApiAcc, ast) =>
            val ic = ImportsContext.fromImports(module.name, ast.imports)
            val (fns, structs, impls) = ast.nodes
              .foldLeft((Seq[syn.FnDefinition](), Seq[syn.StructDefinition](), Seq[syn.StructImplementation]())) {
                case ((fns, structs, impls), it) =>
                  it match {
                    case fn: syn.FnDefinition => (fns :+ fn, structs, impls)
                    case st: syn.StructDefinition => (fns, structs :+ st, impls)
                    case impl: syn.StructImplementation => (fns, structs, impls :+ impl)
                  }
              }

            val unimplementedStructs = structs.map {
              st =>
                val unimplSt = StructAnalyzer.convertStructure(st, ic)
                unimplSt.fullTpe.name -> unimplSt
            }.toMap

            val structImpls = impls.foldLeft(Map[String, Map[String, FnSignature]]()) {
              (implsMap, impl) =>
                val (stName, moduleName) = ic.resolveName(impl.structType.name)
                if (moduleName != module.name) {
                  throw new RuntimeException(s"Implementing structure $stName from module $moduleName in module ${module.name}")
                }
                if (!unimplementedStructs.contains(stName)) {
                  throw new RuntimeException(s"Cannot implement $stName in module $moduleName: structure isn't defined in current file")
                }
                val sigs = impl.methods.map(fn => FnAnalyzer.convertSignature(fn.signature, ic))
                val sigsMap = sigs.foldLeft(Map[String, FnSignature]()) {
                  (acc, it) =>
                    if (acc.contains(it.name)) {
                      throw new RuntimeException(s"Duplicated method ${it.name} for structure $stName")
                    }
                    acc + (it.name -> it)
                }
                if (implsMap.contains(stName)) {
                  throw new RuntimeException(s"Duplicated implementations for structure $stName")
                }
                implsMap + (stName -> sigsMap)
            }
            val implementedStructs = unimplementedStructs.values.map {
              st =>
                st.copy(methods = structImpls.getOrElse(st.fullTpe.name, Map()))
            }
            val moduleApiAccWithStructs = implementedStructs.foldLeft(moduleApiAcc)(_ withStruct _)

            fns.foldLeft(moduleApiAccWithStructs) {
              (api, fn) =>
                api.withFunction(FnAnalyzer.convertSignature(fn.signature, ic))
            }
        }
    }
  }

  def makeModules(moduleAPIs: ModulesTree[ModuleAPI], sources: ModulesTree[Seq[SyntaxTree]]): ModulesTree[ModuleImpl] = {
    val moduleAPIsMap = moduleAPIs.fold(Map[ModuleName, ModuleAPI]()) {
      (acc, it) =>
        acc + (it.name -> it.payload)
    }

    sources.map {
      module =>
        val ast = module.payload
        val (convertedFns, convertedStructImpls) = ast.foldLeft((Seq[sem.FnDefinition](), Map[String, Seq[sem.FnDefinition]]())) {
          case ((moduleFns, moduleImpls), synTree) =>
            val ic = ImportsContext.fromImports(module.name, synTree.imports)
            synTree.nodes.foldLeft((moduleFns, moduleImpls)) {
              case ((fnsAcc, implsAcc), definition) =>
                definition match {
                  case fn: syn.FnDefinition =>
                    val fnConverter = new FnConverter(ic, moduleAPIsMap, new VarIdGen)
                    (fnsAcc :+ fnConverter.convert(fn), implsAcc)
                  case impl: syn.StructImplementation =>
                    val implMethods = impl.methods.map {
                      implMethod =>
                        val fnConverter = new FnConverter(ic, moduleAPIsMap, new VarIdGen)
                        fnConverter.convertImpl(implMethod, impl.structType)
                    }
                    val structName = ic.resolveName(impl.structType.name)._1
                    (fnsAcc, implsAcc + (structName -> implMethods))
                  case sd: syn.StructDefinition => (fnsAcc, implsAcc)
                }
            }
        }
        ModuleImpl(module.name, moduleAPIsMap(module.name).structs.values.toSeq, convertedFns, convertedStructImpls)
    }
  }

  case class DefinitionWithContext(definition: syn.Definition, ic: ImportsContext)

}

object ModuleConverter extends ModuleConverter