package ru.avhaliullin.whatever.semantic

import org.apache.bcel.generic.{ArrayType, ObjectType, Type}
import ru.avhaliullin.whatever.common.ClassName

/**
  * @author avhaliullin
  */
class JavaTypeGen(className: ClassName) {

  def toObjectType(tpe: Tpe): ObjectType = {
    toJavaType(tpe) match {
      case ot: ObjectType => ot
      case _ => throw new RuntimeException(s"Type $tpe isn't an object type")
    }
  }

  def toJavaType(s: Structure): ObjectType = {
    new ObjectType(className.name + "$" + s.name)
  }

  def toJavaType(tpe: Tpe): Type = {
    tpe match {
      case Tpe.BOOL => Type.BOOLEAN
      case Tpe.INT => Type.INT
      case Tpe.UNIT => Type.VOID
      case Tpe.ARGS => new ArrayType(Type.STRING, 1)
      case Tpe.Struct(name) => new ObjectType(className.name + "$" + name)
      case Tpe.STRING => Type.STRING
    }
  }

  def toPrimitiveOrObject(tpe: Tpe): Type = {
    toJavaType(tpe) match {
      case t: ObjectType => Type.OBJECT
      case other => other
    }
  }

}
