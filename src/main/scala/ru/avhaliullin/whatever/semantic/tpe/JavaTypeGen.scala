package ru.avhaliullin.whatever.semantic.tpe

import org.apache.bcel.generic.{ArrayType, ObjectType, Type}
import ru.avhaliullin.whatever.semantic.Structure
import ru.avhaliullin.whatever.semantic.module.ModuleName

/**
  * @author avhaliullin
  */
class JavaTypeGen {

  def toObjectType(tpe: Tpe): ObjectType = {
    toJavaFnRetType(tpe) match {
      case ot: ObjectType => ot
      case _ => throw new RuntimeException(s"Type $tpe isn't an object type")
    }
  }

  def toArrayType(tpe: Tpe): ArrayType = {
    toJavaType(tpe) match {
      case at: ArrayType => at
      case _ => throw new RuntimeException(s"Type $tpe isn't an array type")
    }
  }

  def moduleToJavaPackage(module: ModuleName): String = {
    module match {
      case ModuleName.Nested(name, ModuleName.ROOT) => name
      case ModuleName.Nested(name, parent) => moduleToJavaPackage(parent) + "." + name
      case ModuleName.ROOT => ""
    }
  }

  def udtToJavaType(udt: Tpe.UDT): ObjectType = {
    new ObjectType(moduleToJavaPackage(udt.module) + "." + udt.name)
  }

  def toJavaType(s: Structure): ObjectType = {
    udtToJavaType(s.fullTpe)
  }

  def toJavaFnRetType(tpe: Tpe): Type = {
    _toJavaType(tpe, true)
  }

  def toJavaType(tpe: Tpe): Type = {
    _toJavaType(tpe, false)
  }

  def moduleObject(module: ModuleName): ObjectType = {
    new ObjectType(moduleToJavaPackage(module) + ".MODULE")
  }

  private def _toJavaType(tpe: Tpe, fnRetType: Boolean): Type = {
    tpe match {
      case Tpe.BOOL => Type.BOOLEAN
      case Tpe.INT => Type.INT
      case Tpe.UNIT => if (fnRetType) Type.VOID else Type.getType(classOf[Void])
      case Tpe.STRING => Type.STRING

      case udt: Tpe.UDT => udtToJavaType(udt)

      case Tpe.Arr(elem) =>
        new ArrayType(toJavaType(elem), 1)
      case Tpe.ANY =>
        throw new RuntimeException("Unexpected: 'Any' isn't implemented - should never be met in _toJavaType")
    }
  }

}

object JavaTypeGen extends JavaTypeGen