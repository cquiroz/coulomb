package coulomb

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

import scala.reflect.macros.blackbox.Context

trait UnitTypeName[T] {
  def name: String // typeName
  def typeString: String
  def =:=[U](t: UnitTypeName[U]): Boolean
}

object UnitTypeName {
  @inline
  def apply[T](implicit ev: UnitTypeName[T]): UnitTypeName[T] = ev

  def unitTypeNameImpl[A: c.WeakTypeTag](
    c: Context
  ): c.Expr[UnitTypeName[A]] = {
    import c.universe._
    val aType = weakTypeOf[A]
    val tpe: TypeName = aType.typeSymbol.asType.name
    val tparam: List[Symbol] = aType.typeSymbol.asType.typeParams
    val tpeName = tparam match {
      case Nil => tpe.toString
      case x => s"${tpe.toString}[${x.map(_.asType.name).mkString(",")}]"
    }

    val src = q"""
      new _root_.coulomb.UnitTypeName[$aType] {
        def name: String = $tpeName
        def typeString: String = name
        def =:=[U](t: UnitTypeName[U]): Boolean = name == t.name
      }
    """

    c.Expr[UnitTypeName[A]](src)
  }

  implicit def unitTypeName[T]: UnitTypeName[T] = macro unitTypeNameImpl[T]
}
