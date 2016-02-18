package ru.avhaliullin.whatever.backend

import org.apache.bcel.Constants
import org.apache.bcel.classfile.{InnerClass, InnerClasses}
import org.apache.bcel.generic.{ConstantPoolGen, ObjectType}

/**
  * @author avhaliullin
  */
object InnerClassHelper {
  def makeRecord(mainClass: String, nestedSimpleName: String, cpg: ConstantPoolGen): InnerClass = {
    val mainTpe = new ObjectType(mainClass)
    val nestedTpe = new ObjectType(mainClass + "$" + nestedSimpleName)

    val mcIdx = cpg.addClass(mainClass)
    val ncIdx = cpg.addClass(nestedTpe)
    val nameIdx = cpg.addUtf8(nestedSimpleName)
    new InnerClass(ncIdx, mcIdx, nameIdx, Constants.ACC_PUBLIC | Constants.ACC_STATIC)
  }

  def makeAttr(records: Seq[InnerClass], cpg: ConstantPoolGen): InnerClasses = {
    val length = records.size * 8 + 2
    val nameIdx = cpg.addUtf8("InnerClasses")
    new InnerClasses(nameIdx, length, records.toArray, cpg.getConstantPool)
  }
}
