package sandbox

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

trait Show[A]:
  def show(a: A): String

object Show:

  inline def summonAll[T <: Tuple]: List[Show[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Show[t]].asInstanceOf[Show[Any]] :: summonAll[ts]

  inline given derived[T](using m: Mirror.Of[T]): Show[T] =
    lazy val name               = constValue[m.MirroredLabel]
    lazy val elemsShowInstances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T] =>
        new Show[T]:
          def show(a: T): String =
            val ord = s.ordinal(a)
            s"${elemsShowInstances(ord).asInstanceOf[Show[T]].show(a)}: ${name}"
      case p: Mirror.ProductOf[T] =>
        new Show[T]:
          def show(a: T): String =
            val elems = elemsShowInstances.iterator
              .zip(a.asInstanceOf[Product].productIterator)
              .map(_.show(_))
            s"${name}(${elems.mkString(", ")})"
end Show

trait Functor[F[_]]:
  def map[A, B](f: A => B)(fa: F[A]): F[B]

given Show[Int] with
  def show(a: Int): String = a.toString

given Show[String] with
  def show(a: String): String = s""""${a}""""

enum Tree[+T] derives Show:
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(value: T)

given Functor[Option] with
  def map[A, B](f: A => B)(fa: Option[A]): Option[B] =
    fa match
      case None    => None
      case Some(a) => Some(f(a))

@main
def derivingTest(): Unit =
  import Tree.*

  val intTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  println(summon[Show[Tree[Int]]].show(intTree))
