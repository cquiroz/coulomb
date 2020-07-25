package coulomb

import scala.reflect.runtime.universe._

trait UnitTypeName[T] {
  def name: String // fully qualified name
  def typeString: String
}

object UnitTypeName {
  @inline
  def apply[T](implicit ev: UnitTypeName[T]): UnitTypeName[T] = ev

  // Implicit builder for `UnitTypeName` instances
  implicit def unitTypeName[T](implicit ut: WeakTypeTag[T]): UnitTypeName[T] =
    new UnitTypeName[T] {
      val wtt = weakTypeOf[T]
      override def name: String = ut.tpe.typeSymbol.name.toString()
      override def typeString: String = {
        def work(t: Type): String = {
          t match { case TypeRef(pre, sym, args) =>
            val ss = sym.toString
                        .stripPrefix("free ")
                        .stripPrefix("trait ")
                        .stripPrefix("class ")
                        .stripPrefix("type ")
            val as = args.map(work)
            if (args.length <= 0) ss else (ss + "[" + as.mkString(",") + "]")
          }
        }
        work(wtt)
      }
    }
}