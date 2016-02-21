package ru.avhaliullin.whatever.backend

import java.io.File
import java.util.jar.{JarEntry, JarOutputStream}

import ru.avhaliullin.whatever.semantic.module.{ModuleImpl, ModulesTree}
import ru.avhaliullin.whatever.semantic.tpe.JavaTypeGen

/**
  * @author avhaliullin
  */
object ModulesCompiler {
  private val moduleClassName = "MODULE"

  private def compileModule(dir: File, module: ModulesTree[ModuleImpl]): Unit = {
    val impl = module.payload
    val classGen = new ClassBytecodeGenerator(module.name, moduleClassName)
    val moduleClass = classGen.generateClass(impl.fns)
    moduleClass.dump(new File(dir, moduleClassName + ".class"))
    impl.structs.foreach {
      st =>
        StructureGenerator.generateStruct(st, impl.structImpls.getOrElse(st.fullTpe.name, Nil)).dump(new File(dir, st.fullTpe.name))
    }
    module.submodules.foreach {
      sub =>
        val subDir = new File(dir, sub.name.name)
        subDir.mkdir()
        compileModule(subDir, sub)
    }
  }

  def compileTo(modules: ModulesTree[ModuleImpl], targetDir: File): Unit = {
    if (!targetDir.exists()) {
      targetDir.mkdir()
    }
    if (!targetDir.isDirectory) {
      throw new RuntimeException(s"Target $targetDir is not a directory")
    }
    targetDir.listFiles().foreach(_.delete())
    compileModule(targetDir, modules)

  }

  def compileTo(module: ModulesTree[ModuleImpl], os: JarOutputStream): Unit = {
    val moduleEntryName = JavaTypeGen.moduleToJavaPackage(module.name).replace('.', '/') + "/"
    val impl = module.payload
    if (impl.fns.nonEmpty) {
      val classGen = new ClassBytecodeGenerator(module.name, moduleClassName)
      val moduleClass = classGen.generateClass(impl.fns)
      os.putNextEntry(new JarEntry(moduleEntryName + moduleClassName + ".class"))
      os.write(moduleClass.getBytes)
      os.closeEntry()
    }
    impl.structs.foreach {
      st =>
        val stName = st.fullTpe.name
        os.putNextEntry(new JarEntry(moduleEntryName + stName + ".class"))
        val jClass = StructureGenerator.generateStruct(st, impl.structImpls.getOrElse(st.fullTpe.name, Nil))
        os.write(jClass.getBytes)
        os.closeEntry()
    }
    module.submodules.foreach {
      sub =>
        val subName = JavaTypeGen.moduleToJavaPackage(sub.name).replace('.', '/') + "/"
        os.putNextEntry(new JarEntry(subName))
        compileTo(sub, os)
    }
  }
}
