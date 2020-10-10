package homework5

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import homework5.Homework5._

class Homework5Spec extends AnyFlatSpec with should.Matchers {
  "HandTexas.create" should "create HandTexas instance if list length is 2" in {
    val card1List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace)))

    HandTexas.create(card1List) shouldBe Some(HandTexas(card1List))
  }

  "HandTexas.create" should "return None if list length is not 2" in {
    val card1List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine)))
    val card2List = CardList(List(Card(Suit.Clubs, Rank.King)))

    HandTexas.create(card1List) shouldBe None
    HandTexas.create(card2List) shouldBe None
  }

  "HandOmaha.create" should "create HandOmaha instance if list length is 4" in {
    val card1List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine), Card(Suit.Spades, Rank.Six)))

    HandOmaha.create(card1List) shouldBe Some(HandOmaha(card1List))
  }

  "HandOmaha.create" should "return None if list length is not 4" in {
    val card1List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine)))
    val card2List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine), Card(Suit.Spades, Rank.Six), Card(Suit.Clubs, Rank.Eight)))

    HandOmaha.create(card1List) shouldBe None
    HandOmaha.create(card2List) shouldBe None
  }

  "Board.create" should "create Board instance if list length is 5" in {
    val card1List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine), Card(Suit.Spades, Rank.Six), Card(Suit.Clubs, Rank.Eight)))

    Board.create(card1List) shouldBe Some(Board(card1List))
  }

  "Board.create" should "return None if list length is not 5" in {
    val card1List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine), Card(Suit.Spades, Rank.Six)))
    val card2List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine), Card(Suit.Spades, Rank.Six), Card(Suit.Clubs, Rank.Eight), Card(Suit.Clubs, Rank.Five)))

    Board.create(card1List) shouldBe None
    Board.create(card2List) shouldBe None
  }

  "CaseCombination.create" should "create CaseCombination instance if everything is correct" in {
    val card1List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine), Card(Suit.Spades, Rank.Six), Card(Suit.Clubs, Rank.Eight)))
    val card2List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine), Card(Suit.Spades, Rank.Six)))
    val card3List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace)))

    CaseCombination.create(PokerCombination.Flush, card1List) shouldBe Right(CaseCombination(PokerCombination.Flush, List(card1List)))
    CaseCombination.create(PokerCombination.FourOfAKind, card2List) shouldBe Right(CaseCombination(PokerCombination.FourOfAKind, List(card2List)))
    CaseCombination.create(PokerCombination.Pair, card3List) shouldBe Right(CaseCombination(PokerCombination.Pair, List(card3List)))

    val card1ListList = List(CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine))), CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace))))
    val card2ListList = List(CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace))), CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace))))

    CaseCombination.create(PokerCombination.FullHouse, card1ListList) shouldBe Right(CaseCombination(PokerCombination.FullHouse, card1ListList))
    CaseCombination.create(PokerCombination.TwoPairs, card2ListList) shouldBe Right(CaseCombination(PokerCombination.TwoPairs, card2ListList))
    CaseCombination.create(PokerCombination.Flush, List(card1List)) shouldBe Right(CaseCombination(PokerCombination.Flush, List(card1List)))
  }

  "CaseCombination.create" should "return Left(ErrorMessage)" in {
    val card1List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine), Card(Suit.Spades, Rank.Six)))
    val card2List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine), Card(Suit.Spades, Rank.Six), Card(Suit.Clubs, Rank.Eight), Card(Suit.Clubs, Rank.Five)))
    val card3List = CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace)))

    CaseCombination.create(PokerCombination.Flush, card1List) shouldBe Left(ErrorMessage("cards.length for Flush should be exactly 5"))
    CaseCombination.create(PokerCombination.Straight, card2List) shouldBe Left(ErrorMessage("cards.length for Straight should be exactly 5"))
    CaseCombination.create(PokerCombination.Pair, card2List) shouldBe Left(ErrorMessage("cards.length for Pair should be exactly 2"))
    CaseCombination.create(PokerCombination.StraightFlush, card3List) shouldBe Left(ErrorMessage("cards.length for StraightFlush should be exactly 5"))

    val card1ListList = List(CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine))), CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace))))
    val card2ListList = List(CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine))))
    val card3ListList = List(CardList(List(Card(Suit.Clubs, Rank.King), Card(Suit.Diamonds, Rank.Ace), Card(Suit.Hearts, Rank.Nine))), CardList(List(Card(Suit.Clubs, Rank.King))))

    CaseCombination.create(PokerCombination.Pair, card1ListList) shouldBe Left(ErrorMessage("elementaryCombinations.length for Pair should be exactly 1"))
    CaseCombination.create(PokerCombination.FullHouse, card2ListList) shouldBe Left(ErrorMessage("elementaryCombinations.length for FullHouse should be exactly 2"))
    CaseCombination.create(PokerCombination.FullHouse, card3ListList) shouldBe Left(ErrorMessage("elementaryCombinations elements lengths for FullHouse should be 3 and 2"))
    CaseCombination.create(PokerCombination.TwoPairs, card1ListList) shouldBe Left(ErrorMessage("elementaryCombinations elements lengths for TwoPairs should be 2 and 2"))
  }
}