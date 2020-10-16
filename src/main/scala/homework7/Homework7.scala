package homework7

import shapeless.ops.fin


// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework7 {

  import cats.data.ValidatedNec
  import cats.syntax.all._
  import scala.util.matching.Regex
  import enumeratum._

  type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

  sealed trait ValidationError
  object ValidationError { 
    case object CardholderNameTooShort extends ValidationError
    case object CardholderNameTooLong extends ValidationError
    case object CardholderNameInvalidFormat extends ValidationError
    
    case object CardNumberInvalidIssuer extends ValidationError
    case object CardNumberInvalidLuhnSum extends ValidationError
    case object CardNumberInvalidFormat extends ValidationError
  }

  class CardholderName private (name: String)
  object CardholderName {
    import ValidationError._

    // cardholder name should have length in range from 2 to 26 characters
    def checkNameLength (name: String): AllErrorsOr[String] =
      if (name.length > 26) CardholderNameTooLong.invalidNec
      else if (name.length < 2) CardholderNameTooShort.invalidNec
      else name.validNec

    // name shouldn't contain anything except letters and there shouldn't
    // be more than 3 words
    def checkNameCharacters (name: String): AllErrorsOr[String] =
      if (name matches "^((?:[A-Za-z]+ ?){1,3})$") name.validNec
      else CardholderNameInvalidFormat.invalidNec

    def apply (name: String): AllErrorsOr[CardholderName] = 
      (checkNameLength(name), checkNameCharacters(name)).mapN(
        (name, _) => new CardholderName(name)
      )
  }

  class CardNumber private (number: String, issuerId: CardNumber.IssuerId)
  object CardNumber{
    import ValidationError._

    sealed trait IssuerId extends EnumEntry {val regex: Regex} 
    object IssuerId extends Enum[IssuerId] {
      val values = findValues

      def apply(number: String): Option[IssuerId] =
        values.find(_.regex matches number)

      case object Visa extends IssuerId {val regex = "^4.+".r} 
      case object MasterCard extends IssuerId {val regex = "^5[1-5].+".r} 
      case object Discover extends IssuerId {val regex = "^((6011)|(644)|(65)).+".r} 
      case object Amex  extends IssuerId{val regex = "^((34)|(37)).+.+".r} 
      /* and so on */ 
    }

    // should have 16 or 15 digits in blocks by 4 separated by space
    // I'll pretend that credit cards with less than 15 digits do not
    // exist to not overcomplicate this homework further
    def checkFormat (number: String): AllErrorsOr[String] =
      if (number matches "^([0-9]{4} ){3}([0-9]{3,4})$") number.validNec
      else CardNumberInvalidFormat.invalidNec
    
    def checkIssuerValidity (number: String): AllErrorsOr[IssuerId] =
      IssuerId(number) match {
        case Some(value) => value.validNec
        case None => CardNumberInvalidIssuer.invalidNec
      }
  
    // algorithm from here:
    // https://7labs.io/tips-tricks/check-validity-of-credit-card-number.html
    def checkNumbersValidity (number: String): AllErrorsOr[String] = {
      val strippedN = number.replaceAll("\\s+", "")
      val appendedN =
        if (strippedN.length == 15) "0" + strippedN 
        else strippedN

      val luhnsSum = appendedN.grouped(2).map(
        str => {
          val doubledEv = (str(0).asDigit * 2)
          (doubledEv / 10 + doubledEv % 10, str(1).asDigit)
        }
      ).foldLeft(0){
        case (acc, (ev, od)) => acc + ev + od
      }

      luhnsSum % 10 == 0 match {
        case true  => number.validNec
        case false => CardNumberInvalidLuhnSum.invalidNec
      }
    }
      
    def apply (number: String): AllErrorsOr[CardNumber] = 
      checkFormat(number) andThen (
        number => (checkNumbersValidity(number), checkIssuerValidity(number)).mapN(
          new CardNumber(_, _)
        )
      )
      
  }

  case class PaymentCard(/* Add parameters as needed */)

  object PaymentCardValidator {


    def validate(
      name: String,
      number: String,
      expirationDate: String,
      securityCode: String,
    ): AllErrorsOr[PaymentCard] = ???
  }
}