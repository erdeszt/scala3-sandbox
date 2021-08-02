package sandbox

import cats.{Applicative, Functor, Monad}
import cats.syntax.functor.given
import cats.syntax.flatMap.given
import cats.effect.IO
import cats.effect.IOApp

import CList.*
import EffectStack.*

// Based on: https://aaronlevin.ca/post/136494428283/extensible-effect-stacks-in-the-van-laarhoven-free

// Note works with C <: AnyKind as well, might be more flexible(less strict though)
enum CList:
  case CNil() extends CList
  case CCons[C[_[_]], T <: CList]() extends CList

type test = CCons[Functor, CCons[Monad, CNil]]

enum EffectStack[A <: CList, M[_]]:
  case EmptyEffect[M[_]]() extends EffectStack[CNil, M]
  case ConsEffect[Effect[_[_]], Effects <: CList, M[_]](
      effect: Effect[M],
      stack:  EffectStack[Effects, M]
  ) extends EffectStack[CCons[Effect, Effects], M]

// Unfortunately this doesn't work. Can't access M[_] in the callback and stack has the wrong inferred type
// TODO: Report dotty issue
// final case class FreeVL2[Effects <: CList, A](runFreeVL: [M[_]] => Monad[M] ?=> EffectStack[Effects, M] => M[A])
// given freeVL2Functor[Effects <: CList]: Functor[[A] =>> FreeVL2[Effects, A]] with
//   def map[A, B](f: A => B)(fa: FreeVL2[Effects, A]): FreeVL2[Effects, B] =
//     // FreeVL2((stack: [M[_]] => Monad[M] ?=> EffectStack[Effects, M]) => fa.runFreeVL(stack).map(f))
//     FreeVL2((stack: [M[_]] => Monad[M] ?=> EffectStack[Effects, M]) => summon[Monad[M]].map(fa.runFreeVL(stack)))

trait FreeVL[Effects <: CList, A]:
  def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[A]

given freeVLFunctor[Effects <: CList]: Functor[[A] =>> FreeVL[Effects, A]] with
  def map[A, B](fa: FreeVL[Effects, A])(f: A => B): FreeVL[Effects, B] =
    new FreeVL[Effects, B]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[B] =
        summon[Functor[M]].map(fa.runFreeVL(stack))(f)

given freeVLApplicative[Effects <: CList](using
    functor: Functor[[A] =>> FreeVL[Effects, A]]
): Applicative[[A] =>> FreeVL[Effects, A]] with
  def pure[A](a: A): FreeVL[Effects, A] =
    new FreeVL[Effects, A]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[A] =
        summon[Monad[M]].pure(a)
  // TODO
  def ap[A, B](ff: FreeVL[Effects, A => B])(fa: FreeVL[Effects, A]): FreeVL[Effects, B] =
    ???

given freeVLMonad[Effects <: CList](using
    applicative: Applicative[[A] =>> FreeVL[Effects, A]]
): Monad[[A] =>> FreeVL[Effects, A]] with
  def pure[A](a: A): FreeVL[Effects, A] =
    applicative.pure(a)
  def flatMap[A, B](fa: FreeVL[Effects, A])(f: A => FreeVL[Effects, B]): FreeVL[Effects, B] =
    new FreeVL[Effects, B]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[B] =
        summon[Monad[M]].flatMap(fa.runFreeVL(stack)) { (a: A) =>
          f(a).runFreeVL(stack)
        }
  def tailRecM[A, B](init: A)(fn: A => FreeVL[Effects, Either[A, B]]): FreeVL[Effects, B] =
    new FreeVL[Effects, B]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[B] =
        summon[Monad[M]].flatMap[Either[A, B], B](fn(init).runFreeVL(stack)) {
          case Left(a)  => tailRecM(a)(fn).runFreeVL(stack)
          case Right(b) => summon[Monad[M]].pure(b)
        }

def interpret[M[_]: Monad, Effects <: CList, A](stack: EffectStack[Effects, M])(program: FreeVL[Effects, A]): M[A] =
  program.runFreeVL(stack)

trait HasEffect[Effects <: CList, Effect[_[_]]]:
  def getEffect[M[_]](stack: EffectStack[Effects, M]): Effect[M]

given hasEffectHere[Effects <: CList, Effect[_[_]]]: HasEffect[CCons[Effect, Effects], Effect] with
  def getEffect[M[_]](stack: EffectStack[CCons[Effect, Effects], M]): Effect[M] =
    stack.asInstanceOf[ConsEffect[Effect, Effects, M]].effect

given hasEffectThere[Effects <: CList, OtherEffect[_[_]], Effect[_[_]]](using
    there: HasEffect[Effects, Effect]
): HasEffect[CCons[OtherEffect, Effects], Effect] with
  def getEffect[M[_]](stack: EffectStack[CCons[OtherEffect, Effects], M]): Effect[M] =
    there.getEffect(stack.asInstanceOf[ConsEffect[OtherEffect, Effects, M]].stack)

def liftVL[Effects <: CList, Effect[_[_]]]: LiftVLApply[Effects, Effect] =
  LiftVLApply[Effects, Effect]()

// If N & M are different then this can go very wrong
class LiftVLApply[Effects <: CList, Effect[_[_]]]():
  def apply[M[_], A](f: Effect[M] => M[A])(using effect: HasEffect[Effects, Effect]): FreeVL[Effects, A] =
    new FreeVL[Effects, A]:
      def runFreeVL[N[_]: Monad](stack: EffectStack[Effects, N]): N[A] =
        f(effect.getEffect[N](stack).asInstanceOf[Effect[M]]).asInstanceOf[N[A]]

trait Http[M[_]]:
  def getHttpEff(url:  String): M[Either[String, String]]
  def postHttpEff(url: String, body: String): M[Either[String, String]]

trait Logging[M[_]]:
  def logEff(message: String): M[Unit]

trait Random[M[_]]:
  def getRandEff: M[Int]

def getHttp[Effects <: CList](url: String)(using
    effect:                        HasEffect[Effects, Http]
): FreeVL[Effects, Either[String, String]] =
  liftVL[Effects, Http](eff => eff.getHttpEff(url))

def postHttp[Effects <: CList](url: String, body: String)(using
    effect:                         HasEffect[Effects, Http]
): FreeVL[Effects, Either[String, String]] =
  liftVL[Effects, Http](eff => eff.postHttpEff(url, body))

def log[Effects <: CList](message: String)(using effect: HasEffect[Effects, Logging]): FreeVL[Effects, Unit] =
  liftVL[Effects, Logging](eff => eff.logEff(message))

def getRand[Effects <: CList](using effect: HasEffect[Effects, Random]): FreeVL[Effects, Int] =
  liftVL[Effects, Random](eff => eff.getRandEff)

def repeatReq[Effects <: CList](url: String)(using
    http:                            HasEffect[Effects, Http],
    logging:                         HasEffect[Effects, Logging],
    random:                          HasEffect[Effects, Random]
): FreeVL[Effects, Unit] =
  for
    numRetries <- getRand
    response <- getHttp(url)
    _ <- log(s"Random: ${numRetries}")
    _ <- log(s"Response: ${response}")
  yield ()

object HttpIO extends Http[IO]:
  def getHttpEff(url: String): IO[Either[String, String]] =
    IO.pure(Right(s"You called: ${url}, it's a success"))
  def postHttpEff(url: String, body: String): IO[Either[String, String]] =
    IO.pure(Left(s"Call to: ${url} failed"))

object LogIO extends Logging[IO]:
  def logEff(message: String): IO[Unit] =
    IO(println(s"[LOG] ${message}"))

object RandomIO extends Random[IO]:
  def getRandEff: IO[Int] =
    IO.pure(4)

type MyEffects = CCons[Http, CCons[Logging, CCons[Random, CNil]]]

val ioInterpreter: EffectStack[MyEffects, IO] =
  ConsEffect(HttpIO, ConsEffect(LogIO, ConsEffect(RandomIO, EmptyEffect[IO]())))

@main
def vleff(): Unit =
  println("YOYOYO")

object FreeVLApp extends IOApp.Simple:
  val run = interpret[IO, MyEffects, Unit](ioInterpreter)(repeatReq("foobar"))
