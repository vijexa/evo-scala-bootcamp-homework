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
    case object CardNumberInvalidIssuerDigitsNumber extends ValidationError
    case object CardNumberInvalidLuhnSum extends ValidationError
    case object CardNumberInvalidFormat extends ValidationError
  }

  class CardholderName private (val name: String)
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

  class CardNumber private (val number: String, val issuerId: CardNumber.IssuerId)
  object CardNumber{
    import scala.language.implicitConversions
    import ValidationError._

    class StrippedString private (val str: String) extends AnyVal
    object StrippedString {
      def apply (str: String) = new StrippedString(str.replaceAll("\\s+", ""))
    }
    implicit val StrippedStringToString: StrippedString => String = _.str

    sealed trait IssuerId extends EnumEntry {
      val regex: Regex
      val digitsN: Range
    } 
    object IssuerId extends Enum[IssuerId] {
      val values = findValues

      def apply(number: StrippedString): Either[ValidationError, IssuerId] =
        values.find(_.regex matches number) match {
          case Some(issuer) => 
            if (issuer.digitsN contains number.length) 
              issuer.asRight
            else CardNumberInvalidIssuerDigitsNumber.asLeft
          case None => CardNumberInvalidIssuer.asLeft
        }
      
      // I guess this is a bit naive approach, but this homework
      // is not intended to be used IRL 😅
      case object Visa extends IssuerId {
        val regex = "^4.+".r
        val digitsN = 16 to 16
      } 
      case object MasterCard extends IssuerId {
        val regex = "^5[1-5].+".r
        val digitsN = 16 to 16
      } 
      case object Discover extends IssuerId {
        val regex = "^((6011)|(644)|(65)).+".r 
        val digitsN = 16 to 19
      } 
      case object Amex extends IssuerId {
        val regex = "^((34)|(37)).+".r
        val digitsN = 15 to 15
      }
      case object Maestro extends IssuerId {
        val regex = "^((50)|(5[6-9])|(6[0-9])).+".r
        val digitsN = 12 to 19
      } 
      /* and so on */ 
    }

    def checkFormat (number: String): AllErrorsOr[StrippedString] = {
      val stripped = StrippedString(number)
      if ((12 to 19 contains stripped.length) && (stripped matches "^[0-9]+$"))
        stripped.validNec
      else CardNumberInvalidFormat.invalidNec
    }
    
    def checkIssuerValidity (number: StrippedString): AllErrorsOr[IssuerId] =
      IssuerId(number) match {
        case Right(issuer) => issuer.validNec
        case Left(err) => err.invalidNec
      }
  
    // algorithm from here:
    // https://7labs.io/tips-tricks/check-validity-of-credit-card-number.html
    def checkNumbersValidity (number: StrippedString): AllErrorsOr[String] = {
      val strippedN = number.str
      val appendedN: String =
        if (strippedN.length % 2 != 0) 
          "0" + strippedN 
        else strippedN

      val luhnsSum = appendedN.grouped(2).map(
        str => {
          val doubledEv = (str(0).asDigit * 2)
          (doubledEv / 10 + doubledEv % 10, str(1).asDigit)
        }
      ).foldLeft(0){
        case (acc, (ev, od)) => acc + ev + od
      }

      if (luhnsSum % 10 == 0) 
        strippedN.validNec
      else 
        CardNumberInvalidLuhnSum.invalidNec
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