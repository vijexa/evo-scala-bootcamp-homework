package homework13.EffectsHomework

import org.scalatest._
import flatspec._
import matchers._

import scala.util.{Success, Failure}

import homework13.EffectsHomework.EffectsHomework.IO
import scala.concurrent.Await

class EffectsHomeworkSpec extends AnyFlatSpec with should.Matchers {

  // IO companion object tests

  "IO.pure" should "return immediately resolved IO" in {
    var effect = 0
    val io = IO.pure(effect += 1)
    
    effect shouldBe 1
    io.unsafeRunSync() shouldBe ()
    effect shouldBe 1
  }

  "IO.apply" should "return delayed IO" in {
    var effect = 0
    val io = IO(effect += 1)

    effect shouldBe 0
    io.unsafeRunSync() shouldBe ()
    effect shouldBe 1
    io.unsafeRunSync() shouldBe ()
    effect shouldBe 2
  }

  "IO.delay" should "be an alias for IO.apply" in {
    IO.delay(2 + 2).unsafeRunSync() shouldBe IO(2 + 2).unsafeRunSync()
  }

  // I'm not so sure about this one
  "IO.suspend" should "suspend the effect" in {
    IO.suspend(IO(2 + 2)).unsafeRunSync() shouldBe 4
  }

  "IO.fromOption" should "create IO from Option" in {
    IO.fromOption(Some(2 + 2))(new Exception).unsafeRunSync() shouldBe 4

    assertThrows[Exception](
      IO.fromOption(None)(new Exception).unsafeRunSync()
    )
  }

  "IO.fromEither" should "create IO from Either" in {
    IO.fromEither(Right(2 + 2)).unsafeRunSync() shouldBe 4

    assertThrows[Exception](
      IO.fromEither(Left(new Exception)).unsafeRunSync()
    )
  }

  "IO.fromTry" should "create IO from Try" in {
    IO.fromTry(Success(2 + 2)).unsafeRunSync() shouldBe 4

    assertThrows[Exception](
      IO.fromTry(Failure(new Exception)).unsafeRunSync()
    )
  }

  "IO.none" should "resolve into None" in {
    IO.none.unsafeRunSync() shouldBe None
  }

  "IO.raiseError" should "resolve into exception" in {
    val io = IO.raiseError(new Exception)

    assertThrows[Exception](io.unsafeRunSync())
  }

  "IO.raiseUnless" should "resolve into exception if condition is not met" in {
    IO.raiseUnless(true)(new Exception).unsafeRunSync() shouldBe ()

    val io = IO.raiseUnless(false)(new Exception)
    assertThrows[Exception](io.unsafeRunSync())
  }

  "IO.raiseWhen" should "resolve into exception if condition is met" in {
    IO.raiseWhen(false)(new Exception).unsafeRunSync() shouldBe ()

    val io = IO.raiseWhen(true)(new Exception)
    assertThrows[Exception](io.unsafeRunSync())
  }

  "IO.unlessA" should "perform action if condition is not met" in {
    var effect = 0

    IO.unlessA(false)(IO(effect += 1)).unsafeRunSync()
    effect shouldBe 1

    IO.unlessA(true)(IO(effect += 1)).unsafeRunSync()
    effect shouldBe 1
  }

  "IO.whenA" should "perform action if condition is met" in {
    var effect = 0

    IO.whenA(true)(IO(effect += 1)).unsafeRunSync()
    effect shouldBe 1

    IO.whenA(false)(IO(effect += 1)).unsafeRunSync()
    effect shouldBe 1
  }

  "IO.unit" should "resolve into unit" in {
    IO.unit.unsafeRunSync() shouldBe ()
  }

  // IO companion class tests

  "IO.unsafeRunSync()" should "run the IO and return value" in {
    IO(2 + 2).unsafeRunSync() shouldBe 4
    IO(println("hello world")).unsafeRunSync() shouldBe ()
  }

  it should "throw an exception if it is unhandled" in {
    val io = IO(throw new Exception)

    assertThrows[Exception](io.unsafeRunSync())
  }

  "IO.map" should "manipulate IO value" in {
    var effect = 0

    val io = IO(2 + 2).map(effect += _).map(_ => effect += 2)
    effect shouldBe 0

    io.unsafeRunSync()
    effect shouldBe 6
  }

  "IO.flatMap" should "compose IO actions" in {
    var effect = 0
    
    val io = IO(2 + 2).flatMap(
      v1 => IO(v1 + 2).flatMap(
        v2 => IO(v2 + 4).flatMap(
          v3 => IO(effect += v3)
        )
      )
    )
    effect shouldBe 0

    io.unsafeRunSync()
    effect shouldBe 10
  }

  "IO.*>" should "compose actions without running effect keeping second IO value" in {
    var effect = 0

    val io = IO(effect += 1) *> IO(2 + 2)
    effect shouldBe 0

    io.unsafeRunSync() shouldBe 4
    effect shouldBe 1
  }

  it should "not run second effect if first one fails" in {
    var effect = 0
    
    val io = IO(throw new Exception) *> IO(effect += 1)
    effect shouldBe 0

    assertThrows[Exception](
      io.unsafeRunSync()
    )
    effect shouldBe 0
  }

  "IO.as" should "replace result of IO with a given value" in {
    var effect = 0

    val io = IO(effect += 1).as{
      effect += 1
      2 + 2
    }

    io.unsafeRunSync() shouldBe 4
    effect shouldBe 2
  }

  it should "not compute effect if IO did not succeed" in {
    var effect = 0

    val io = IO{
      effect += 1
      throw new Exception
    }.as{
      effect += 1
      2 + 2
    }
    effect shouldBe 0
    
    assertThrows[Exception](
      io.unsafeRunSync()
    )
    effect shouldBe 1
  }

  "IO.void" should "replace IO result with unit" in {
    var effect = 0

    val io = IO{
      effect += 1
      2 + 2
    }
    effect shouldBe 0

    io.unsafeRunSync() shouldBe 4
    effect shouldBe 1

    io.void.unsafeRunSync() shouldBe ()
    effect shouldBe 2
  }

  "IO.attempt" should "resolve into Right if IO is succesful" in {
    IO(2 + 2).attempt.unsafeRunSync() shouldBe Right(4)
  }

  it should "resolve into Left if IO is failed" in {
    IO(throw new Exception).attempt.unsafeRunSync().isLeft shouldBe true
  }

  "IO.redeem" should "execute recover function if IO is failed" in {
    IO[Int](throw new Exception)
      .redeem(_ => "failed", _ => "successful")
      .unsafeRunSync() shouldBe "failed"
  }

  it should "execute map function if IO is successful" in {
    IO(2 + 2)
      .redeem(_ => "failed", _ => "successful")
      .unsafeRunSync() shouldBe "successful"
  }

  "IO.option" should "return a Some if IO is successful" in {
    IO(2 + 2).option.unsafeRunSync() shouldBe Some(4)
  }

  it should "return a None if IO is failed" in {
    IO(throw new Exception).option.unsafeRunSync() shouldBe None
  }

  "IO.handleErrorWith" should "return correct value if IO is successful" in {
    IO(2 + 2).handleErrorWith(_ => IO(0)).unsafeRunSync() shouldBe 4
  }

  it should "execute recover function if IO is failed" in {
    IO(throw new Exception).handleErrorWith(_ => IO(0)).unsafeRunSync() shouldBe 0
  }

  "IO.redeemWith" should "execute bind function if IO is successful" in {
    IO(2 + 2).redeemWith(_ => IO(0), _ => IO(100)).unsafeRunSync() shouldBe 100
  }

  it should "execute recover function if IO is failed" in {
    IO[Int](throw new Exception).redeemWith(_ => IO(0), _ => IO(100)).unsafeRunSync() shouldBe 0
  }

  "IO.unsafeToFuture" should "return future that resolves in success" in {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    IO(2 + 2).unsafeToFuture() onComplete {
      case Success(value)     => value shouldBe 4
      case Failure(exception) => fail(s"future resolved in failure: $exception")
    } 
  }

  "IO.unsafeToFuture" should "return future that resolves in failure" in {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    IO[Int](throw new Exception).unsafeToFuture() onComplete {
      case Success(value)     => fail("future resolved in success")
      case Failure(exception) => assertThrows[Exception](throw exception)
    } 
  }
}