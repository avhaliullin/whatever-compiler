package ru.avhaliullin.whatever.semantic.module

import ru.avhaliullin.whatever.frontend.sources.{SourceTree, SourceTreeNode}
import ru.avhaliullin.whatever.frontend.syntax.{SyntaxTree, SyntaxTreeNode => syn}
import ru.avhaliullin.whatever.semantic.function.{FnAnalyzer, FnConverter}
import ru.avhaliullin.whatever.semantic.{ImportsContext, SemanticTreeNode => sem, StructAnalyzer, VarIdGen}

/**
  * @author avhaliullin
  */
class ModuleConverter {

  private def makeModulesAPI(sources: Seq[SourceTreeNode[SyntaxTree]], curModule: ModuleName): ModulesTree[(ModuleAPI, Seq[DefinitionInContext])] = {
    val (allSubmodules, moduleApi, moduleDefs) =
      sources.foldLeft((Seq[ModulesTree[(ModuleAPI, Seq[DefinitionInContext])]](), ModuleAPI(curModule, Map(), Map()), Seq[DefinitionInContext]())) {
        case ((submodules, moduleApiAcc, moduleDefsAcc), src) =>
          src match {
            case SourceTreeNode.SrcFile(_, _, ast) =>
              val ic = ImportsContext.fromImports(curModule, ast.imports)
              val (newModuleApiAcc, newModuleDefsAcc) = ast.nodes.foldLeft((moduleApiAcc, moduleDefsAcc)) {
                case ((moduleApi, moduleDefs), node) =>
                  node match {
                    case fn@syn.FnDefinition(sig, _) =>
                      moduleApi.withFunction(FnAnalyzer.convertSignature(sig, ic)) -> (moduleDefs :+ DefinitionInContext(fn, ic))
                    case sd: syn.StructDefinition =>
                      moduleApi.withStruct(StructAnalyzer.convertStructure(sd, ic)) -> moduleDefs
                  }
              }
              (submodules, newModuleApiAcc, newModuleDefsAcc)

            case SourceTreeNode.SrcDirectory(name, children, _) =>
              val newModuleName = curModule.sub(name)
              val newSubmodulesAcc = submodules :+ makeModulesAPI(children, newModuleName)
              (newSubmodulesAcc, moduleApiAcc, moduleDefsAcc)
          }
      }
    ModulesTree(moduleApi.name, allSubmodules, (moduleApi, moduleDefs))
  }

  def makeModulesAPI(sources: SourceTree[SyntaxTree]): ModulesTree[(ModuleAPI, Seq[DefinitionInContext])] = {
    makeModulesAPI(sources.sources, ModuleName.ROOT)
  }

  def makeModules(modules: ModulesTree[(ModuleAPI, Seq[DefinitionInContext])]): ModulesTree[ModuleImpl] = {
    val moduleAPIs = modules.fold(Map[ModuleName, ModuleAPI]()) {
      (acc, it) =>
        acc + (it.name -> it.payload._1)
    }

    modules.map {
      module =>
        val (api, fns) = module.payload
        val convertedFns = fns.map {
          fn =>
            val fnConverter = new FnConverter(fn.ic, moduleAPIs, new VarIdGen)
            fnConverter.convert(fn.definition.code, api.functions(fn.definition.signature.name))
        }
        ModuleImpl(module.name, api.structs.values.toSeq, convertedFns)
    }
  }

  case class DefinitionInContext(definition: syn.FnDefinition, ic: ImportsContext)

}

object ModuleConverter extends ModuleConverter