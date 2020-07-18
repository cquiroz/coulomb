package coulomb.cats

import cats._
import cats.implicits._
import coulomb.Quantity

trait CatsImplicits {
  /**
    * Eq[Quantity[V, U]] when there is an Eq[V]
    *
    * @group typeclass
    */
  implicit def eqQuantity[V: Eq, U]: Eq[Quantity[V, U]] = Eq.by(_.value)

  /**
    * Order[Quantity[V, U]] when there is an Order[V]
    *
    * @group typeclass
    */
  implicit def orderQuantity[V: Order, U]: Order[Quantity[V, U]] = Order.by(_.value)

}

object implicits extends CatsImplicits
