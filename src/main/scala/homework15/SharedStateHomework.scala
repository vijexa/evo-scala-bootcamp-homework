package homework15

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Tip: you can use following structure to get current time suspended in effect : Clock[F].realTime(MILLISECONDS).flatMap(...)
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 *
 * If we will put a value with the same key then it should renew expiration
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]

    def remove(key: K): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
    state: Ref[F, Map[K, (Long, V)]],
    expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {

    def checkExpirationRepeatedly(
      interval: FiniteDuration
    )(
      implicit T: Timer[F], C: Concurrent[F]
    ): F[Unit] = {
      state.update(
        _.collect{
          case (key, (exp, value)) if exp > 0 => (key, (exp - interval.length, value))
        }
      ) >> T.sleep(interval).flatMap(
        _ => checkExpirationRepeatedly(interval)(T, C)
      )
    }

    def get(key: K): F[Option[V]] = state.get.map(_.get(key).map{case (_, v) => v})

    def put(key: K, value: V): F[Unit] = state.update(
      m => (
        m + (key -> (expiresIn.length -> value))
      )
    )

    def remove(key: K): F[Unit] = state.update(
      m => (
        m.removed(key)
      )
    )

  }

  object Cache {
    def of[F[_] : Clock, K, V](
      expiresIn: FiniteDuration,
      checkOnExpirationsEvery: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {

      C.delay(
        new RefCache[F, K, V](
          Ref.unsafe[
            F, 
            Map[K, (Long, V)]
          ](Map.empty),
          expiresIn
        )
      ).flatMap(
        cache => {
          C.start(
            cache.checkExpirationRepeatedly(
              checkOnExpirationsEvery
            )(T, C)
          ) as cache
        }
      )
    }
      

  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](6.seconds, 1.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(4.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(4.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }
}

