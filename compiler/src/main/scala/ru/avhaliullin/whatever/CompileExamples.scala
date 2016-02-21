package ru.avhaliullin.whatever

import java.io.File

import ru.avhaliullin.whatever.backend.ModulesCompiler
import ru.avhaliullin.whatever.frontend.Frontend
import ru.avhaliullin.whatever.semantic.module.ModuleConverter

/**
  * @author avhaliullin
  */
object CompileExamples {

  def main(args: Array[String]): Unit = {
    val examplesDir = new File("examples")
    val srcDir = new File(examplesDir, "src")
    val targetDir = new File(examplesDir, "target")
    val classesDir = new File(targetDir, "classes")
    val jarFile = new File(targetDir, "examples.jar")
    if (!targetDir.exists()) {
      targetDir.mkdir()
    }
    val srcTree = Frontend.parseSources(srcDir)
    val modulesOfSources = ModuleConverter.mapSourcesToModules(srcTree)
    val moduleApis = ModuleConverter.makeModulesAPI(modulesOfSources)
    val moduleASTs = ModuleConverter.makeModules(moduleApis, modulesOfSources)
    ModulesCompiler.compileTo(moduleASTs, classesDir)
    //    val fOut = new FileOutputStream(jarFile)
    //    val jarOut = new JarOutputStream(fOut)
    //
    //    ModulesCompiler.compileTo(moduleASTs, jarOut)
    //    jarOut.close()
    //    fOut.close()
  }
}
