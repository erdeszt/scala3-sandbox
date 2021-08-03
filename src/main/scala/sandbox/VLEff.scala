package sandbox

import cats.{Applicative, Functor, Monad}
import cats.syntax.functor.given
import cats.syntax.flatMap.given
import cats.effect.{IO, IOApp}

import CList.*
import EffectStack.*

// Based on: https://aaronlevin.ca/post/136494428283/extensible-effect-stacks-in-the-van-laarhoven-free

/* Type level list of types that take type constructors as arguments.
 * For example: CCons[Functor, CCons[Applicative,
 * CCons[Monad, CNil]]]
 */
enum CList:
  case CNil() extends CList
  case CCons[C[_[_]], T <: CList]() extends CList

/* List of effects that our program can use .
 * Indexed by the list of effects and the underlyig monad
 */
enum EffectStack[A <: CList, M[_]]:
  case EmptyEffect[M[_]]() extends EffectStack[CNil, M]
  case ConsEffect[Effect[_[_]], Effects <: CList, M[_]](
      effect: Effect[M],
      stack:  EffectStack[Effects, M]
  ) extends EffectStack[CCons[Effect, Effects], M]

/* Van-Laarhoven free monad.
 *
 * Unfortunately the case clas encoding doesn't work.
 * Can't access M[_] in the callback and stack has the wrong inferred type.
 * final case class FreeVL2[Effects <: CList, A](runFreeVL: [M[_]] => Monad[M] ?=> EffectStack[Effects, M] => M[A])
 * given freeVL2Functor[Effects <: CList]: Functor[[A] =>> FreeVL2[Effects, A]] with
 *   def map[A, B](f: A => B)(fa: FreeVL2[Effects, A]): FreeVL2[Effects, B] =
 *     FreeVL2((stack: [M[_]] => Monad[M] ?=> EffectStack[Effects, M]) => summon[Monad[M]].map(fa.runFreeVL(stack)))
 */
trait FreeVL[Effects <: CList, A]:
  def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[A]

given freeVLFunctor[Effects <: CList]: Functor[[A] =>> FreeVL[Effects, A]] with
  def map[A, B](fa: FreeVL[Effects, A])(f: A => B): FreeVL[Effects, B] =
    new FreeVL[Effects, B]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[B] =
        summon[Functor[M]].map(fa.runFreeVL(stack))(f)

given freeVLApplicative[Effects <: CList]: Applicative[[A] =>> FreeVL[Effects, A]] with
  def pure[A](a: A): FreeVL[Effects, A] =
    new FreeVL[Effects, A]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[A] =
        summon[Monad[M]].pure(a)
  def ap[A, B](ff: FreeVL[Effects, A => B])(fa: FreeVL[Effects, A]): FreeVL[Effects, B] =
    new FreeVL[Effects, B]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[B] =
        summon[Monad[M]].flatMap(ff.runFreeVL(stack)) { (f: A => B) =>
          summon[Monad[M]].map(fa.runFreeVL(stack)) { (a: A) =>
            f(a)
          }
        }

given freeVLMonad[Effects <: CList]: Monad[[A] =>> FreeVL[Effects, A]] with
  def pure[A](a: A): FreeVL[Effects, A] =
    freeVLApplicative.pure(a)
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

/* Execute a FreeVL program with the given effects stack
 */
def interpret[M[_]: Monad, Effects <: CList, A](stack: EffectStack[Effects, M])(program: FreeVL[Effects, A]): M[A] =
  program.runFreeVL(stack)

/* Extracting parts of an effect stack without specifying the whole stack.
 * It's a linear search in the EffectStack.
 */
trait HasEffect[Effects <: CList, Effect[_[_]]]:
  def getEffect[M[_]](stack: EffectStack[Effects, M]): Effect[M]

/* The top of the effect stack is the effect we are looking for
 */
given hasEffectHere[Effects <: CList, Effect[_[_]]]: HasEffect[CCons[Effect, Effects], Effect] with
  def getEffect[M[_]](stack: EffectStack[CCons[Effect, Effects], M]): Effect[M] =
    stack match
      case ConsEffect(effect, _) => effect

/* The top of the stack is NOT the effect we are looking for but we have
 * evidence that the tail contains it.
 */
given hasEffectThere[Effects <: CList, OtherEffect[_[_]], Effect[_[_]]](using
    there: HasEffect[Effects, Effect]
): HasEffect[CCons[OtherEffect, Effects], Effect] with
  def getEffect[M[_]](stack: EffectStack[CCons[OtherEffect, Effects], M]): Effect[M] =
    stack match
      case ConsEffect(_, tail) => there.getEffect(tail)

/* Lift an operation into the correct place in the effect stack
 */
def liftVL[Effects <: CList, Effect[_[_]]]: LiftVLApply[Effects, Effect] =
  LiftVLApply[Effects, Effect]()

/* Work around for rank 2 polymorphism for M[_]
 */
class LiftVLApply[Effects <: CList, Effect[_[_]]]():
  def apply[A](f: Lifted[Effects, Effect, A])(using effect: HasEffect[Effects, Effect]): FreeVL[Effects, A] =
    new FreeVL[Effects, A]:
      def runFreeVL[M[_]: Monad](stack: EffectStack[Effects, M]): M[A] =
        f(effect.getEffect(stack))

trait Lifted[Effects <: CList, Effect[_[_]], A]:
  def apply[M[_]](eff: Effect[M]): M[A]

/* ---==== Example effects & programs: ====--- */

/* -= Effect defintions */

trait Http[M[_]]:
  def getHttpEff(url:  String): M[Either[String, String]]
  def postHttpEff(url: String, body: String): M[Either[String, String]]

trait Logging[M[_]]:
  def logEff(message: String): M[Unit]

trait Random[M[_]]:
  def getRandEff: M[Int]

/* -= Wrapper boilerplate, could be generated with a macro =- */

def getHttp[Effects <: CList](url: String)(using
    effect:                        HasEffect[Effects, Http]
): FreeVL[Effects, Either[String, String]] =
  liftVL(new Lifted[Effects, Http, Either[String, String]] {
    def apply[M[_]](eff: Http[M]): M[Either[String, String]] =
      eff.getHttpEff(url)
  })

def postHttp[Effects <: CList](url: String, body: String)(using
    effect:                         HasEffect[Effects, Http]
): FreeVL[Effects, Either[String, String]] =
  liftVL(new Lifted[Effects, Http, Either[String, String]] {
    def apply[M[_]](eff: Http[M]): M[Either[String, String]] =
      eff.postHttpEff(url, body)
  })

def log[Effects <: CList](message: String)(using effect: HasEffect[Effects, Logging]): FreeVL[Effects, Unit] =
  liftVL(new Lifted[Effects, Logging, Unit] {
    def apply[M[_]](eff: Logging[M]): M[Unit] =
      eff.logEff(message)
  })

def getRand[Effects <: CList](using effect: HasEffect[Effects, Random]): FreeVL[Effects, Int] =
  liftVL(new Lifted[Effects, Random, Int] {
    def apply[M[_]](eff: Random[M]): M[Int] =
      eff.getRandEff
  })

/* -= Example program version 1, explicit using statements for the effects =- */

def exampleProgram[Effects <: CList](url: String)(using
    http:                                 HasEffect[Effects, Http],
    logging:                              HasEffect[Effects, Logging],
    random:                               HasEffect[Effects, Random]
): FreeVL[Effects, Unit] =
  for
    numRetries <- getRand
    response <- getHttp(url)
    _ <- log(s"Random: ${numRetries}")
    _ <- log(s"Response: ${response}")
  yield ()

/* -= Example program version 2 with type aliases and context bounds =- */

type HasHttp[Effects <: CList]    = HasEffect[Effects, Http]
type HasLogging[Effects <: CList] = HasEffect[Effects, Logging]
type HasRandom[Effects <: CList]  = HasEffect[Effects, Random]

def exampleProgram2[Effects <: CList: HasHttp: HasLogging: HasRandom](url: String): FreeVL[Effects, Unit] =
  for
    numRetries <- getRand
    response <- getHttp(url)
    _ <- log(s"Random: ${numRetries}")
    _ <- log(s"Response: ${response}")
  yield ()

/* -= Implementation of the different effects using IO =- */

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

/* -= The full effect stack and interpreter for it =- */
type MyEffects = CCons[Http, CCons[Logging, CCons[Random, CNil]]]

val ioInterpreter: EffectStack[MyEffects, IO] =
  ConsEffect(HttpIO, ConsEffect(LogIO, ConsEffect(RandomIO, EmptyEffect[IO]())))

object FreeVLApp extends IOApp.Simple:
  val run = interpret[IO, MyEffects, Unit](ioInterpreter)(exampleProgram("http://foo.bar"))
