package homework13.EffectsHomework

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

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
object EffectsHomework {
  final class IO[A] private (private val run: () => A) {
    def map[B](f: A => B): IO[B] = IO(f(run()))

    def flatMap[B](f: A => IO[B]): IO[B] = IO.suspend(f(run()))

    def *>[B](another: IO[B]): IO[B] = map(_ => another.run())

    def as[B](newValue: => B): IO[B] = map(_ => newValue)

    def void: IO[Unit] = map(_ => ())

    def attempt: IO[Either[Throwable, A]] =  IO(
      Try(run()) match {
        case Failure(exception) => Left(exception)
        case Success(value)     => Right(value)
      }
    )

    def option: IO[Option[A]] = redeem(_ => None, Some(_))

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = attempt.flatMap{
      case Left(value)  => f(value)
      case Right(value) => IO(value)
    }

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = attempt.map{
      case Left(value)  => recover(value)
      case Right(value) => map(value)
    }

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = attempt.flatMap{
      case Left(value)  => recover(value)
      case Right(value) => bind(value)
    }

    def unsafeRunSync(): A = run()

    def unsafeToFuture(): Future[A] = Future(run())(scala.concurrent.ExecutionContext.global)
  }

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)
    
    def suspend[A](thunk: => IO[A]): IO[A] = delay(thunk.run())

    def delay[A](body: => A): IO[A] = new IO(() => body)

    def pure[A](a: A): IO[A] = IO(a)

    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Left(err)    => raiseError(err)
      case Right(value) => IO(value)
    }

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case Some(value) => IO(value)
      case None        => raiseError(orElse)
    }

    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Failure(exception) => raiseError(exception)
      case Success(value)     => IO(value)
    }

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