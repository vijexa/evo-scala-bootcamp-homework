package homework12

import java.net.URL
import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.io.Source
import scala.concurrent.Await
import scala.util.Success
import scala.util.Failure

/**
 * Application:
 * - takes a web-page URL from arguments (args array)
 * - loads the web-page body, extracts HTTP links from it
 * - for all the found links, tries to fetch a server name header if there is one
 * - prints all the encountered unique server name values in alphabetical order
 *
 * Each link processing should be done in parallel.
 * Validation of arguments is not needed.
 *
 * Try to test it on http://google.com!
 */
object Homework12 extends App {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val url = args.headOption match {
    case Some(value) => value
    case None        => "https://google.com"
  }

  getServerNameForEveryLink(url).onComplete{
    case Success(value) => println(s"\nServer names:\n${value.toList.sortBy(_.head.toLower).mkString(", ")}")
    case Failure(exception) => println(s"\nOops, we messed up:\n$exception")
  }

  def getServerNameForEveryLink (
    url: String
  ): Future[Set[String]] = {
    val futureTask = (
      for {
        body   <- fetchPageBody(url)
        urlSet <- findLinkUrls(body)
      } yield Future.sequence(
        urlSet map fetchServerName
      )
    ).flatten

    futureTask.map(_.flatten)
  }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }


  private def findLinkUrls(html: String): Future[Set[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toSet
  }
}
