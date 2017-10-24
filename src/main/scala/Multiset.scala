sealed trait Multiset[+A] {

  def filter(f: A => Boolean): Multiset[A] = {
    this match {
      case Element(head, tail) =>
        if (f(head)) {
          Element(head, tail.filter(f))
        } else {
          tail.filter(f)
        }
      case Empty => Empty
    }
  }

  def map[B](f: A => B): Multiset[B] = {
    this match {
      case Element(head, tail) => Element(f(head), tail.map(f))
      case Empty => Empty
    }
  }

  def flatMap[B](f: A => Multiset[B]): Multiset[B] = {
    this match {
      case Element(head, tail) => f(head) | tail.flatMap(f)
      case Empty => Empty
    }
  }

  def find[B >: A](v: B): Option[A] = {
    this match {
      case Element(head, tail) =>
        if (head == v) {
          Some(head)
        } else {
          tail.find(v)
        }
      case Empty => None
    }

  }

  def apply[B >: A](v: B): Boolean = {
    find(v).isDefined
  }

  def &[B >: A](m: Multiset[B]): Multiset[B] = {
    this match {
      case Element(head, tail) =>
        if (m(head)) {
          Element(head, tail & m)
        } else {
          tail & m
        }
      case Empty => Empty
    }
  }

  def |[B >: A](m: Multiset[B]): Multiset[B] = {
    this match {
      case Element(head, tail) => Element(head, tail | m)
      case Empty => m
    }
  }

  def flatten[B >: A](): Seq[B]
}

case class Element[+A](head: A, tail: Multiset[A]) extends Multiset[A] {
  override def flatten[B >: A](): Seq[B] = List(head) ++ tail.flatten()
}

case object Empty extends Multiset[Nothing] {
  override def flatten[B](): Seq[B] = List.empty
}

object Multiset {
  def apply[A](seq: A*): Multiset[A] = {
    if (seq.isEmpty) {
      Empty
    } else {
      Element(seq.head, apply(seq.tail: _*))
    }
  }

  def unapplySeq[A](seq: Multiset[A]): Option[Seq[A]] = {
    seq match {
      case Element(head, tail) => Some(List(head) ++ tail.flatten())
      case Empty => None
    }
  }

}

object Main extends App {
  Multiset(1, 2, 3, 4, 5).map(x => print(x))
}
