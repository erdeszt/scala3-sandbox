package sandbox

enum CList:
  case CNil() extends CList
  case CCons[C[_[_]], T <: CList]() extends CList

import CList.*

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]
trait Applicative[F[_]]:
  // TODO: <*>
  def pure[A](a: A): F[A]
trait Monad[F[_]] extends Functor[F]:
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

type test = CCons[Functor, CCons[Monad, CNil]]

enum EffectStack[A <: CList, M[_]]:
  case EmptyEffect[M[_]]() extends EffectStack[CNil, M]
  case ConsEffect[Effect[_[_]], Effects <: CList, M[_]](
      effect: Effect[M],
      stack:  EffectStack[Effects, M]
  ) extends EffectStack[CCons[Effect, Effects], M]

final case class FreeVL[Effects <: CList, A](runFreeVL: [M[_]] => EffectStack[Effects, M] => M[A])
// trait FreeVL[Effects <: CList, A]:
//   def runFreeVL[M[_]](stack: EffectStack[Effects, M]): M[A]

given freeVLFunctor[Effects <: CList]: Functor[[A] =>> FreeVL[Effects, A]] with
  def map[A, B](fa: FreeVL[Effects, A])(f: A => B): FreeVL[Effects, B] =
    null.asInstanceOf[FreeVL[Effects, B]]

@main
def vleff(): Unit =
  println("YOYOYO")
