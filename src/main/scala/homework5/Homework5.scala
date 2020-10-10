package homework5
import homework5.Homework5.PokerCombination.HighCard
import homework5.Homework5.PokerCombination.Pair
import homework5.Homework5.PokerCombination.TwoPairs
import homework5.Homework5.PokerCombination.ThreeOfAKind
import homework5.Homework5.PokerCombination.Straight
import homework5.Homework5.PokerCombination.Flush
import homework5.Homework5.PokerCombination.FullHouse
import homework5.Homework5.PokerCombination.FourOfAKind
import homework5.Homework5.PokerCombination.StraightFlush
import akka.http.scaladsl.server.util.BinaryPolyFunc.Case
import shapeless.ops.fin
import scala.collection.immutable.Nil
import scala.util.chaining._

object Homework5 {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  final case class ErrorMessage (error: String)


  sealed trait Suit {
    def representation: Char
  }
  object Suit {
    case object Clubs extends Suit {
      val representation = 'c'
    }

    case object Diamonds extends Suit {
      val representation = 'd'
    }

    case object Hearts extends Suit {
      val representation = 'h'
    }

    case object Spades extends Suit {
      val representation = 's'
    }
  }


  sealed trait Rank {
    def representation: Char
    def value: Int
  }
  object Rank {
    case object Two extends Rank {
      val representation = '2'
      val value = 2
    }
    
    case object Three extends Rank {
      val representation = '3'
      val value = 3
    }
    
    case object Four extends Rank {
      val representation = '4'
      val value = 4
    }
    
    case object Five extends Rank {
      val representation = '5'
      val value = 5
    }
    
    case object Six extends Rank {
      val representation = '6'
      val value = 6
    }
    
    case object Seven extends Rank {
      val representation = '7'
      val value = 7
    }
    
    case object Eight extends Rank {
      val representation = '8'
      val value = 8
    }
    
    case object Nine extends Rank {
      val representation = '9'
      val value = 9
    }
    
    case object Ten extends Rank {
      val representation = 'T'
      val value = 10
    }
    
    case object Jack extends Rank {
      val representation = 'J'
      val value = 11
    }
    
    case object Queen extends Rank {
      val representation = 'Q'
      val value = 12
    }
    
    case object King extends Rank {
      val representation = 'K'
      val value = 13
    }
    
    case object Ace extends Rank {
      val representation = 'A'
      val value = 14
    }
  }


  final case class Card (suit: Suit, rank: Rank)
  final case class CardList (list: List[Card]) 

  sealed trait Hand
  sealed trait CardContainer {
    def cards: CardList
  }
  sealed trait CardContainerCompanion {
    def containerLength: Int
    def apply(cards: CardList): CardContainer
    def create(cards: CardList): Option[CardContainer] = 
      if (cards.list.length == containerLength) Some(apply(cards))
      else None
  }

  final case class HandTexas (cards: CardList) extends Hand with CardContainer
  object HandTexas extends CardContainerCompanion {
    val containerLength = 2
  }

  final case class HandOmaha (cards: CardList) extends Hand with CardContainer
  object HandOmaha extends CardContainerCompanion {
    val containerLength = 4
  }


  final case class Board (cards: CardList) extends CardContainer
  object Board extends CardContainerCompanion {
    val containerLength = 5
  }

  sealed trait PokerCombination {
    def power: Int
    def firstElement: ElementaryPokerCombination
    def secondElement: Option[ElementaryPokerCombination]
  }
  object PokerCombination {
    case object HighCard extends PokerCombination {
      val power = 0
      val firstElement = ElementaryPokerCombination.HighCard
      val secondElement = None
    }

    case object Pair extends PokerCombination {
      val power = 1
      val firstElement = ElementaryPokerCombination.Pair
      val secondElement = None
    }

    case object TwoPairs extends PokerCombination {
      val power = 2
      val firstElement = ElementaryPokerCombination.Pair
      val secondElement = Some(ElementaryPokerCombination.Pair)
    }

    case object ThreeOfAKind extends PokerCombination {
      val power = 3
      val firstElement = ElementaryPokerCombination.ThreeOfAKind
      val secondElement = None
    }

    case object Straight extends PokerCombination {
      val power = 4
      val firstElement = ElementaryPokerCombination.Straight
      val secondElement = None
    }

    case object Flush extends PokerCombination {
      val power = 5
      val firstElement = ElementaryPokerCombination.Flush
      val secondElement = None
    }

    case object FullHouse extends PokerCombination {
      val power = 6
      val firstElement = ElementaryPokerCombination.ThreeOfAKind
      val secondElement = Some(ElementaryPokerCombination.Pair)
    }

    case object FourOfAKind extends PokerCombination {
      val power = 7
      val firstElement = ElementaryPokerCombination.FourOfAKind
      val secondElement = None
    }

    case object StraightFlush extends PokerCombination {
      val power = 8
      val firstElement = ElementaryPokerCombination.StraightFlush
      val secondElement = None
    }
  }

  // those are combinations that create real poker combinations
  // e.g. Full House is a combination of Three of a Kind and a Pair
  sealed trait ElementaryPokerCombination {
    def cardsLength: Int
  }
  object ElementaryPokerCombination {
    case object HighCard extends ElementaryPokerCombination {
      val cardsLength = 5
    }

    case object Pair extends ElementaryPokerCombination {
      val cardsLength = 2
    }

    case object ThreeOfAKind extends ElementaryPokerCombination {
      val cardsLength = 3
    }

    case object Straight extends ElementaryPokerCombination {
      val cardsLength = 5
    }

    case object Flush extends ElementaryPokerCombination {
      val cardsLength = 5
    }

    case object FourOfAKind extends ElementaryPokerCombination {
      val cardsLength = 4
    }

    case object StraightFlush extends ElementaryPokerCombination {
      val cardsLength = 5
    }
  }


  final case class CaseCombination (comb: PokerCombination, cards: List[CardList]) {
    def firstElementaryComb = cards.head
    def secondElementaryComb: Option[CardList] = cards.tail.headOption
  }
  case object CaseCombination {
    def create(comb: PokerCombination, cards: CardList): Either[ErrorMessage, CaseCombination] = comb match {
      case TwoPairs | FullHouse => Left(ErrorMessage(s"Please use 'CaseCombination.create(comb:PokerCombination, cards:List[CardList])' to create CaseCombination for $comb"))
      case _ => {
        if (comb.firstElement.cardsLength == cards.list.length) Right(CaseCombination(comb, List(cards)))
        else Left(ErrorMessage(s"cards.length for $comb should be exactly ${comb.firstElement.cardsLength}"))
      }
    }

    def create(comb: PokerCombination, elementaryCombinations: List[CardList]): Either[ErrorMessage, CaseCombination] = comb match {
      case comb @ (TwoPairs | FullHouse) => elementaryCombinations match { 
        case firstEl :: secondEl :: Nil =>  
          if (firstEl.list.length == comb.firstElement.cardsLength && secondEl.list.length == comb.secondElement.get.cardsLength) Right(CaseCombination(comb, elementaryCombinations))
          else Left(ErrorMessage(s"elementaryCombinations elements lengths for $comb should be ${comb.firstElement.cardsLength} and ${comb.secondElement.get.cardsLength}"))
        case _ => Left(ErrorMessage(s"elementaryCombinations.length for $comb should be exactly 2"))
      }
      case _ => elementaryCombinations match {
        case el :: Nil => create(comb, el)
        case _ => Left(ErrorMessage(s"elementaryCombinations.length for $comb should be exactly 1"))
      }
    }
  }

}
