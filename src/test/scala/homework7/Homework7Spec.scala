package homework7

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import cats.data.Chain
import cats.syntax.all._

import scala.util.chaining._

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
    CardNumber("6100 0000 0000 2").isValid shouldBe true
    CardNumber("6100 0000 0006").isValid shouldBe true
    CardNumber("610000000006").isValid shouldBe true
    CardNumber("6100 0000 0000 0000 002").isValid shouldBe true
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
      
    CardNumber("6100 0000 0000 0000 0006") shouldBe 
      Invalid(Chain(CardNumberInvalidFormat))
    
    // mastercard should be only 16 digits long
    CardNumber("5500 0000 0000 04") shouldBe 
      Invalid(Chain(CardNumberInvalidIssuerDigitsNumber))
  }

  "CardNumber.issuerId" should "be correct" in {
    import homework7.Homework7.CardNumber.IssuerId._

    CardNumber("5500 0000 0000 0004").valueOr(null).issuerId shouldBe MasterCard
    CardNumber("4417 1234 5678 9113").valueOr(null).issuerId shouldBe Visa
    CardNumber("3400 0000 0000 009").valueOr(null).issuerId shouldBe Amex
  }

  {
    import java.time.format.DateTimeFormatter
    import java.time.YearMonth

    val format = DateTimeFormatter.ofPattern("MM/yy")

    def parseAndValidate (expDate: String, currDate: String) = 
      ExpirationDate(expDate, YearMonth.parse(currDate, format))//.tap(println)

    "ExpirationDate.apply" should "return validated ExpirationDate" in {
      parseAndValidate("09/20", "08/20").isValid shouldBe true
      parseAndValidate("08/19", "06/17").isValid shouldBe true
      parseAndValidate("10/78", "03/60").isValid shouldBe true
      parseAndValidate("12/20", "12/20").isValid shouldBe true
    }

    "ExpirationDate.apply" should "return correct chain of errors" in {
      parseAndValidate("sdf", "12/20") shouldBe 
        Invalid(Chain(ExpirationDateInvalidFormat))

      parseAndValidate("13/20", "12/20") shouldBe 
        Invalid(Chain(ExpirationDateInvalidFormat))

      parseAndValidate("10/2020", "12/20") shouldBe 
        Invalid(Chain(ExpirationDateInvalidFormat))

      parseAndValidate("00/20", "12/20") shouldBe 
        Invalid(Chain(ExpirationDateInvalidFormat))

      parseAndValidate("2/20", "12/20") shouldBe 
        Invalid(Chain(ExpirationDateInvalidFormat))


      parseAndValidate("11/20", "12/20") shouldBe 
        Invalid(Chain(ExpirationDateExpired))
      parseAndValidate("11/20", "12/56") shouldBe 
        Invalid(Chain(ExpirationDateExpired))
    }
  }

  {
    import CardNumber.IssuerId._

    "SecurityCode.apply" should "return validated security code" in {
      SecurityCode("123", MasterCard.some).isValid shouldBe true
      SecurityCode("345", Visa.some).isValid shouldBe true
      SecurityCode("4736", Amex.some).isValid shouldBe true
    }

    "SecurityCode.apply" should "return correct chain of errors" in {
      SecurityCode("123", Amex.some) shouldBe 
        Invalid(Chain(SecurityCodeInvalidLength))

      SecurityCode("3455", Visa.some) shouldBe 
        Invalid(Chain(SecurityCodeInvalidLength))

      SecurityCode("4736", MasterCard.some) shouldBe 
        Invalid(Chain(SecurityCodeInvalidLength))

        
      SecurityCode("2hg45", Discover.some) shouldBe 
        Invalid(Chain(SecurityCodeInvalidFormat))

      SecurityCode("", Discover.some) shouldBe 
        Invalid(Chain(SecurityCodeInvalidFormat))

      SecurityCode("12", Discover.some) shouldBe 
        Invalid(Chain(SecurityCodeInvalidFormat))

      SecurityCode("dfhj", None) shouldBe 
        Invalid(Chain(SecurityCodeInvalidFormat))
    }
  }

  {
    import CardNumber.IssuerId._

    "PaymentCard.apply" should "return validated PaymentCard" in {
      PaymentCard("Name Surname", "4417 1234 5678 9113", "12/40", "123").isValid shouldBe true
      PaymentCard("Name Surname Middlename", "610000000006", "12/50", "764").isValid shouldBe true
    }

    "PaymentCard.apply" should "return correct chain of errors" in {
      PaymentCard("Name Surname Middlename adgad", "610000000006", "12/50", "764") shouldBe 
        Invalid(Chain(CardholderNameTooLong, CardholderNameInvalidFormat))

      PaymentCard("1", "610000000006", "12/50", "764") shouldBe 
        Invalid(Chain(CardholderNameTooShort, CardholderNameInvalidFormat)) 

      PaymentCard("Name Surname Middlename adgad", "610000000006", "12/50", "7645") shouldBe 
        Invalid(Chain(CardholderNameTooLong, CardholderNameInvalidFormat, SecurityCodeInvalidLength))

      PaymentCard("Name Surname Middlename adgad", "610000000006", "12/10", "7645") shouldBe 
        Invalid(
          Chain(
            CardholderNameTooLong, 
            CardholderNameInvalidFormat, 
            ExpirationDateExpired, 
            SecurityCodeInvalidLength
          )
        )

      PaymentCard("Name Surname Middlename adgad", "610000000006", "12/10", "sdhfg") shouldBe 
        Invalid(
          Chain(
            CardholderNameTooLong, 
            CardholderNameInvalidFormat, 
            ExpirationDateExpired,
            SecurityCodeInvalidFormat
          )
        )

      PaymentCard("Name Surname Middlename adgad", "6100000006", "12/10", "7645") shouldBe 
        Invalid(
          Chain(
            CardholderNameTooLong, 
            CardholderNameInvalidFormat, 
            CardNumberInvalidFormat, 
            ExpirationDateExpired
          )
        )
      
      PaymentCard("Name Surname Middlename adgad", "6100000006", "12/10", "dsgfg") shouldBe 
        Invalid(
          Chain(
            CardholderNameTooLong, 
            CardholderNameInvalidFormat, 
            CardNumberInvalidFormat, 
            ExpirationDateExpired,
            SecurityCodeInvalidFormat
          )
        )
      
      PaymentCard("Name Surname Middlename adgad", "6100000006", "dfgh", "dsgfg") shouldBe 
        Invalid(
          Chain(
            CardholderNameTooLong, 
            CardholderNameInvalidFormat, 
            CardNumberInvalidFormat, 
            ExpirationDateInvalidFormat,
            SecurityCodeInvalidFormat
          )
        )
    }
  }
  
}