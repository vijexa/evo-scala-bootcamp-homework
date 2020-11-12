package homework14.EffectsHomeworkDeclarative

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomeworkDeclarative {

  private sealed trait Trampoline[A] {
    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = Trampoline.MoreMap(this, f)
  }

  private object Trampoline {

    final case class Done[A] (v: A) extends Trampoline[A]
    final case class More[A] (next: () => Trampoline[A]) extends Trampoline[A]
    final case class MoreMap[A, B] (sub: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]


    @tailrec
    def runTrampolined[A](tramp: Trampoline[A]): A = {
      tramp match {
        case Done(v) => v 
        case More(next) => runTrampolined(next())
        case MoreMap(sub, f1) => sub match {
          case Done(v) => runTrampolined(f1(v))
          case More(next) => runTrampolined(MoreMap(next(), f1))
          case MoreMap(sub, f2) => runTrampolined(
            sub.flatMap(f2(_).flatMap(f1))
          )
        }
      }
    }

  }



  private final case class Pure[A] (v: A) extends IO[A]
  
  private final case class Delay[A] (f: () => A) extends IO[A]

  private final case class Suspend[A] (f: () => IO[A]) extends IO[A]

  private final case class Map[A, B] (f: A => B, io: IO[A]) extends IO[B]
  
  private final case class FlatMap[A, B] (f: A => IO[B], io: IO[A]) extends IO[B]

  private final case class Attempt[A] (io: IO[A]) extends IO[Either[Throwable, A]]

  class IO[A] {

    def map[B](f: A => B): IO[B] = Map(f, this)

    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(f, this)

    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)

    def as[B](newValue: => B): IO[B] = map(_ => newValue)

    def void: IO[Unit] = map(_ => ())

    def attempt: IO[Either[Throwable, A]] = Attempt(this)

    def option: IO[Option[A]] = redeem(_ => None, Some(_))

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = 
      attempt.flatMap(_.fold(f, IO.apply(_)))

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = 
      attempt.map(_.fold(recover, map))

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = 
      attempt.flatMap(_.fold(recover, bind))

    def unsafeRunSync(): A = Trampoline.runTrampolined(IO.interpret(this))

    def unsafeToFuture(): Future[A] = 
      Future(unsafeRunSync())(scala.concurrent.ExecutionContext.global)
  }



  object IO {

    private def interpret[A] (io: IO[A]): Trampoline[A] = {
      import Trampoline._
      io match {
        case Pure(v)        => Done(v)
        case Delay(f)       => Done(f())
        case Suspend(f)     => More(() => interpret(f()))
        case Map(f, io)     => MoreMap(More(() => interpret(io)), (v: Any) => Done(f(v)))
        case FlatMap(f, io) => MoreMap(More(() => interpret(io)), (v: Any) => interpret(f(v)))
        case Attempt(io)    => Try(interpret(io)) match {
          case Failure(exception) => Done(Left(exception))
          case Success(value) => MoreMap(value, (v: Any) => Done(Right(v)))
        }
      }
    }

    def apply[A](body: => A): IO[A] = delay(body)
    
    def suspend[A](thunk: => IO[A]): IO[A] = Suspend(() => thunk)

    def delay[A](body: => A): IO[A] = Delay(() => body)

    def pure[A](a: A): IO[A] = Pure(a)

    def fromEither[A](e: Either[Throwable, A]): IO[A] = 
      e.fold(raiseError, apply(_))

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = 
      option.fold(raiseError[A](orElse))(apply(_))

    def fromTry[A](t: Try[A]): IO[A] = 
      t.fold(raiseError, apply(_))

    def none[A]: IO[Option[A]] = pure(None)

    def raiseError[A](e: Throwable): IO[A] = IO(throw e)

    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = 
      if (!cond) raiseError(e) else unit

    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = 
      if (cond) raiseError(e) else unit

    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = 
      if (!cond) action else unit

    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = 
      if (cond) action else unit

    val unit: IO[Unit] = pure(())
  }
}
