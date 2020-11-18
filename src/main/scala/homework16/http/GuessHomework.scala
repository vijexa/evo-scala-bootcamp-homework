package homework16.http

import org.http4s._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.multipart.{Multipart, Part}
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.circe.CirceEntityCodec._

import io.circe.generic.auto._
import io.circe.syntax._

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.effect.ExitCode
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.chaining._

import java.util.UUID

import homework15.SharedStateHomework._

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.

object JsonObjects {
  case class GameProcessJson (humanMessage: String, verdict: String)
  case class GameErrorJson (humanMessage: String, error: String)
  case class GameStartJson (humanMessage: String)
}

object GuessServer extends IOApp {

  case class GameStatus (attemptsLeft: Int, number: Long)
  
  object QueryMatchers {
    object MinNumMatcher extends QueryParamDecoderMatcher[Long](name = "min")
    object MaxNumMatcher extends QueryParamDecoderMatcher[Long](name = "max")
    object AttemptsMatcher extends QueryParamDecoderMatcher[Int](name = "attempts")
    object GuessMatcher extends QueryParamDecoderMatcher[Long](name = "guess")
  }

  def gameProcess (game: GameStatus, guess: Long, id: String, clients: Cache[IO, String, GameStatus]) = {
    import JsonObjects._

    val GameStatus(attempts, number) = game
    if (attempts > 0) {
      val verdict: String = 
        if      (number > guess) "larger"
        else if (number < guess) "smaller"
        else "equal"
        
      if (number == guess)
        clients.remove(id) *>
        Ok(GameProcessJson(s"You've won the game! 🎉🎉🎉\n" +
          s"The number was $number and you had $attempts attempts left!", verdict).asJson)
      else {
        val newAttempts = attempts - 1
        clients.put(id, GameStatus(newAttempts, number)) *>
        Ok(GameProcessJson(s"The number is $verdict, you have ${newAttempts} attempts left", verdict).asJson)
      }
    } else {
      clients.remove(id) *> 
      Ok(GameProcessJson(s"You are out of attempts! Try starting the game again! " +
      s"The number was $number btw", "game over").asJson)
    }
  }

  def gameGuessRoute (clients: Cache[IO, String, GameStatus]) = {
    import QueryMatchers.GuessMatcher
    import JsonObjects.GameErrorJson

    HttpRoutes.of[IO] {
      case req @ GET -> Root / "guess" :? GuessMatcher(guess) => {
        req.cookies
          .find(_.name == "id")
          .map{
            case RequestCookie(_, id) => 
              clients.get(id).map{
                case Some(game) => gameProcess(game, guess, id, clients)
                case None => Forbidden(
                  GameErrorJson(s"I can't find you in my database! Maybe your time is out?", "wrong id").asJson)
              }.flatten
          }
          .fold(Forbidden(
            GameErrorJson("I need cookiez! Go to /start to start the game!", "no id").asJson)
          )(identity)
      }
    }
  }

  def gameStartRoute (clients: Cache[IO, String, GameStatus]) = {
    import QueryMatchers._
    import JsonObjects.GameStartJson
    
    HttpRoutes.of[IO] {
      case GET -> Root / "start" :? MinNumMatcher(minNum) :? MaxNumMatcher(maxNum) :? AttemptsMatcher(attempts) => {
        val id = UUID.randomUUID().toString
        val number = scala.util.Random.between(minNum, maxNum)

        clients.put(id, GameStatus(attempts, number)) *>
        Ok(
          GameStartJson(s"Your new id is $id, write it down! (in your 🍪 pls)\n" +
          s"Your number is between $minNum and $maxNum. You have $attempts attempts\n" +
          s"You'll lose if you are inactive for 10 minutes!\n$number").asJson
        ).map(_.addCookie("id", id))
      }
    }
  }

  private[http] def httpApp (clients: Cache[IO, String, GameStatus]) = {
    gameStartRoute(clients) <+> gameGuessRoute(clients)
  }.orNotFound

  def run (args: List[String]): IO[ExitCode] = {
    for {
      clientsCache <- Cache.of[IO, String, GameStatus](10.minutes, 1.minute)
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(httpApp(clientsCache))
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } yield (ExitCode.Success)
  }
} 


object GuessClient extends IOApp {
  import JsonObjects._
  import org.http4s.client._

  private val uri = uri"http://localhost:9001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private def avg (a: Long, b: Long): Long = (a + b) / 2

  // binary search algorithm should guess it in 100% of cases!
  val minNumber = 0L
  val maxNumber = 1000L
  val attempts = 10

  def gameStart(client: Client[IO]): IO[String] = {
    client.get(
      (uri / "start").withQueryParams(
        Map(
          ("min" -> minNumber.toString),
          ("max" -> maxNumber.toString),
          ("attempts" -> attempts.toString)
        )
      )
    ) (
      resp => resp.cookies
        .find(_.name == "id")
        .fold(
          throw new Exception("wow this is exceptional, I can't get id")
        )(cookie => IO.pure(cookie.content))
    )
  }

  def gameProcess(
    client: Client[IO], 
    id: String,
    currNumber: Long,
    minNumber: Long,
    maxNumber: Long
  ): IO[Unit] = {
    client.expect[GameProcessJson](
      GET(
        (uri / "guess").withQueryParam(
          "guess", currNumber
        )
      ).map(_.addCookie("id", id))
    ).flatMap(
      resp => printLine(s"\n\nTrying to guess $currNumber, got answer: ${resp.humanMessage}\n\n") *> (resp.verdict match {
        case "smaller" => gameProcess(client, id, avg(minNumber, currNumber), minNumber, currNumber)
        case "larger" => gameProcess(client, id, avg(currNumber, maxNumber), currNumber, maxNumber)
        case "equal" => IO.unit
        case "game over" => IO.unit
        case verdict @ _ => IO.raiseError(new Exception(s"Got unexpected verdict: $verdict"))
      })
    )
  }

  def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO]).use { case (client, blocker) =>
      for {
        _   <- printLine(string = "Starting game and getting id:")
        id  <- gameStart(client)
        _   <- printLine()
        _   <- printLine(s"new id is: $id")
        _   <- printLine()
        _   <- printLine()
        _   <- gameProcess(client, id, avg(minNumber, maxNumber), minNumber, maxNumber)
        _   <- printLine()
      } yield ExitCode.Success
    }
  }
}