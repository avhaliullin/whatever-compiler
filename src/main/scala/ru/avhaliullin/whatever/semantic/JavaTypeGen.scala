package ru.avhaliullin.whatever.semantic

import org.apache.bcel.generic.{ArrayType, ObjectType, Type}
import ru.avhaliullin.whatever.common.ClassName

/**
  * @author avhaliullin
  */
class JavaTypeGen(className: ClassName) {

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

  def toJavaType(s: Structure): ObjectType = {
    new ObjectType(className.name + "$" + s.name)
  }

  def toJavaFnRetType(tpe: Tpe): Type = {
    _toJavaType(tpe, true)
  }

  def toJavaType(tpe: Tpe): Type = {
    _toJavaType(tpe, false)
  }

  private def _toJavaType(tpe: Tpe, fnRetType: Boolean): Type = {
    tpe match {
      case Tpe.BOOL => Type.BOOLEAN
      case Tpe.INT => Type.INT
      case Tpe.UNIT => if (fnRetType) Type.VOID else Type.getType(classOf[Void])
      case Tpe.ARGS => new ArrayType(Type.STRING, 1)
      case Tpe.STRING => Type.STRING

      case Tpe.Struct(name) => new ObjectType(className.name + "$" + name)

      case Tpe.Arr(elem) =>
        new ArrayType(toJavaType(elem), 1)
      case Tpe.ANY =>
        throw new RuntimeException("Unexpected: 'Any' isn't implemented - should never be met in _toJavaType")
    }
  }

}
