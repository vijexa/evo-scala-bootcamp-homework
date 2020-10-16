package homework7

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import cats.data.Chain

class Homework7Spec extends AnyFlatSpec with should.Matchers {
  import homework7.Homework7._
  import homework7.Homework7.ValidationError._

  "CardHolderName.apply" should "return validated CardHolderName" in {
    CardholderName("NAME SURNAME").isValid shouldBe true
    CardholderName("NAME MIDDLENAME SURNAME").isValid shouldBe true
  }

  "CardHolderName.apply" should "return correct chain of errors" in {
    CardholderName("NAME MIDDLENAME SURNAME something") shouldBe 
      Invalid(Chain(CardholderNameTooLong, CardholderNameInvalidFormat))

    CardholderName("1") shouldBe 
      Invalid(Chain(CardholderNameTooShort, CardholderNameInvalidFormat))

    CardholderName("          ") shouldBe 
      Invalid(Chain(CardholderNameInvalidFormat))

    CardholderName("    name surname      ") shouldBe 
      Invalid(Chain(CardholderNameInvalidFormat))
  }

  "CardNumber.apply" should "return validated CardNumber" in {
    CardNumber("4417 1234 5678 9113").isValid shouldBe true
    CardNumber("4111 1111 1111 1111").isValid shouldBe true
    CardNumber("5500 0000 0000 0004").isValid shouldBe true
    CardNumber("6440 0000 0000 0005").isValid shouldBe true
    CardNumber("3400 0000 0000 009").isValid shouldBe true
  }

  "CardNumber.apply" should "return correct chain of errors" in {
    CardNumber("1234 5678 1010 2020") shouldBe 
      Invalid(Chain(CardNumberInvalidLuhnSum, CardNumberInvalidIssuer))
      
    CardNumber("0000 0000 0000 0000") shouldBe 
      Invalid(Chain(CardNumberInvalidIssuer))

    CardNumber("5500 0000 0000 0002") shouldBe 
      Invalid(Chain(CardNumberInvalidLuhnSum))
      
    CardNumber("1234 56f8 1010 2020") shouldBe 
      Invalid(Chain(CardNumberInvalidFormat))

    CardNumber("12320") shouldBe 
      Invalid(Chain(CardNumberInvalidFormat))

    CardNumber("") shouldBe 
      Invalid(Chain(CardNumberInvalidFormat))

    CardNumber("4417 1234 5678 9113 1212") shouldBe 
      Invalid(Chain(CardNumberInvalidFormat))
  }

  "CardNumber.issuerId" should "be correct" in {
    import homework7.Homework7.CardNumber.IssuerId._

    CardNumber("5500 0000 0000 0004").valueOr(null).issuerId shouldBe MasterCard
    CardNumber("4417 1234 5678 9113").valueOr(null).issuerId shouldBe Visa
    CardNumber("3400 0000 0000 009").valueOr(null).issuerId shouldBe Amex
  }
}