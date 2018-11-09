package fsis

trait Equal[A] {
  def isEqual(l: A, r: A): Boolean
}

object Equal {
  def instance[A](f: (A, A) => Boolean): Equal[A] = (l: A, r: A) => f(l, r)

  def natural[A]: Equal[A] = instance(_ == _)

  implicit val eqInt: Equal[Int] = natural
  implicit val eqLong: Equal[Long] = natural
  implicit val eqString: Equal[String] = natural

  implicit def eqList[A](implicit eq: Equal[A]): Equal[List[A]] = instance { (lhs, rhs) =>
    lhs.length == rhs.length &&
      lhs.zip(rhs).forall {
        case (l, r) => eq.isEqual(l, r)
      }
  }

  implicit def eqOption[A](implicit eq: Equal[A]): Equal[Option[A]] = instance { (lhs, rhs) =>
    (lhs, rhs) match {
      case (Some(l), Some(r)) => eq.isEqual(l, r)
      case (None, None)       => true
      case _                  => false
    }
  }
}

case class IsEq[A](l: A, r: A) {
  def isEqual(implicit eq: Equal[A]): Boolean = eq.isEqual(l, r)
}

object IsEq {

  implicit class Syntax[A](l: A) {
    def =?=(r: A): IsEq[A] = IsEq(l, r)
  }

}