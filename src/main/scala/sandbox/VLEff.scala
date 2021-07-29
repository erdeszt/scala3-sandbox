package sandbox

enum CList:
  case CNil() extends CList
  case CCons[C[_[_]], T <: CList]() extends CList

import CList.*

trait Functor[F[_]]:
  def map[A, B](f: A => B)(fa: F[A]): F[B]
trait Applicative[F[_]] extends Functor[F]:
  // TODO: <*>
  def pure[A](a: A): F[A]
trait Monad[F[_]] extends Applicative[F]:
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

type test = CCons[Functor, CCons[Monad, CNil]]

enum EffectStack[A <: CList, M[_]]:
  case EmptyEffect[M[_]]() extends EffectStack[CNil, M]
  case ConsEffect[Effect[_[_]], Effects <: CList, M[_]](
      effect: Effect[M],
      stack:  EffectStack[Effects, M]
  ) extends EffectStack[CCons[Effect, Effects], M]

extension [F[_]: Functor, A](fa: F[A])(using functor: Functor[F])
  def fmap[B](f: A => B): F[B] =
    functor.map(f)(fa)

// Unfortunately this doesn't work. Can't access M[_] in the callback and stack has the wrong inferred type
// TODO: Report dotty issue
// final case class FreeVL2[Effects <: CList, A](runFreeVL: [M[_]] => Monad[M] ?=> EffectStack[Effects, M] => M[A])
// given freeVL2Functor[Effects <: CList]: Functor[[A] =>> FreeVL2[Effects, A]] with
//   def map[A, B](f: A => B)(fa: FreeVL2[Effects, A]): FreeVL2[Effects, B] =
//     // FreeVL2((stack: [M[_]] => Monad[M] ?=> EffectStack[Effects, M]) => fa.runFreeVL(stack).fmap(f))
//     FreeVL2((stack: [M[_]] => Monad[M] ?=> EffectStack[Effects, M]) => summon[Monad[M]].map(fa.runFreeVL(stack)))

trait FreeVL[Effects <: CList, A]:
  def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[A]

given freeVLFunctor[Effects <: CList]: Functor[[A] =>> FreeVL[Effects, A]] with
  def map[A, B](f: A => B)(fa: FreeVL[Effects, A]): FreeVL[Effects, B] =
    new FreeVL[Effects, B]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[B] =
        summon[Functor[M]].map(f)(fa.runFreeVL(stack))

given freeVLApplicative[Effects <: CList](using
    functor: Functor[[A] =>> FreeVL[Effects, A]]
): Applicative[[A] =>> FreeVL[Effects, A]] with
  def map[A, B](f: A => B)(fa: FreeVL[Effects, A]): FreeVL[Effects, B] =
    functor.map(f)(fa)
  def pure[A](a: A): FreeVL[Effects, A] =
    new FreeVL[Effects, A]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[A] =
        summon[Monad[M]].pure(a)

given freeVLMonad[Effects <: CList](using
    applicative: Applicative[[A] =>> FreeVL[Effects, A]]
): Monad[[A] =>> FreeVL[Effects, A]] with
  def map[A, B](f: A => B)(fa: FreeVL[Effects, A]): FreeVL[Effects, B] =
    applicative.map(f)(fa)
  def pure[A](a: A): FreeVL[Effects, A] =
    applicative.pure(a)
  def flatMap[A, B](fa: FreeVL[Effects, A])(f: A => FreeVL[Effects, B]): FreeVL[Effects, B] =
    ???

@main
def vleff(): Unit =
  println("YOYOYO")
