package game

import com.petuxbot.domain.Rank.{Ace, Jack, King, Queen, Ten}
import com.petuxbot.domain.Suit.{Clubs, Diamonds, Hearts, Spades}
import com.petuxbot.domain.cardcontainers.Hand
import com.petuxbot.domain.{Card, Player, Score, StrongestCard}
import com.petuxbot.game.CardValidator._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CardValidatorSpec extends AnyFlatSpec with Matchers {
  val card1: Card = Card(Queen, Hearts)
  val card2: Card = Card(Jack, Hearts)
  val card3: Card = Card(Ace, Clubs)
  val card4: Card = Card(Ace, Hearts)
  val cardNotFromHand: Card = Card(Ten, Clubs)
  val trump: Card = Card(Queen, Spades, isTrump = true)
  val trumpHigh: Card = Card(Ace, Spades, isTrump = true)
  val highScores = List(Score(15),Score(4))
  val lowScores = List(Score(15),Score(3))
  val playerWithTrump: Player =
    Player(0, "Player", Hand(List(card1, card2, card3, card4, trump)), Score(15), List.empty)

  val playerWithTrumps: Player =
    Player(0, "Player", Hand(List(card1, card2, card3, trump, trumpHigh)), Score(15), List.empty)

  val playerWithoutTrump: Player =
    Player(0, "Player", Hand(List(card1, card2)), Score(15), List.empty)

  val playerWithoutRequiredSuitAndWithoutTrump: Player =
    Player(0, "Player", Hand(List(card1, card2)), Score(15), List.empty)

  val strongestCardNotTrump: StrongestCard = StrongestCard(Card(King, Hearts), 1)
  val strongestCardNotTrumpUnique: StrongestCard = StrongestCard(Card(Queen, Diamonds), 1)
  val strongestCardTrump: StrongestCard = StrongestCard(Card(King, Spades, isTrump = true), 1)
  val cardToHitNotTrump: Card = Card(Queen, Hearts)
  val cardToHitNotTrumpUnique: Card = Card(Queen, Diamonds)
  val cardToHitTrump: Card = Card(Ten, Spades, isTrump = true)


  "isCardValidToMakeTurn" should "validate any card from hand to make turn with " +
    "if score of all players above 3 points" in {
    isCardValidToMakeTurn(card1, playerWithTrump, highScores) shouldEqual true
    isCardValidToMakeTurn(trump, playerWithTrump, highScores) shouldEqual true
  }

  "isCardValidToMakeTurn" should "validate trump card from hand to make turn with " +
    "if score of any player is under or equal 3 points" in {
    isCardValidToMakeTurn(trump, playerWithTrump, lowScores) shouldEqual true
  }

  "isCardValidToMakeTurn" should "not validate card from hand to make turn with " +
    "if score of any player is under or equal 3 points" in {
    isCardValidToMakeTurn(card1, playerWithTrump, lowScores) shouldEqual false
  }

  "isCardValidToMakeTurn" should "not validate card to make turn with " +
    "if player does not have such a card in hand" in {
    isCardValidToMakeTurn(cardNotFromHand, playerWithTrump, lowScores) shouldEqual false
  }

  "isCardValidToMakeTurn" should "validate any card to make turn with " +
    "if player does not have trumps" in {
    isCardValidToMakeTurn(card1, playerWithoutTrump, lowScores) shouldEqual true
    isCardValidToMakeTurn(card2, playerWithoutTrump, lowScores) shouldEqual true
  }

  "isCardValidToMakeTurn" should "validate any card to make turn with  " +
    "if player does not have required suit or trump" in {
    isCardValidToMakeTurn(card1, playerWithoutTrump, lowScores) shouldEqual true
    isCardValidToMakeTurn(card2, playerWithoutTrump, lowScores) shouldEqual true
  }

  "isCardValidToMakeAttack" should "not validate card to make attack with " +
    "if player does not have such a card in hand" in {
    isCardValidToMakeAttack(
      cardNotFromHand,
      playerWithTrump,
      strongestCardNotTrump,
      cardToHitNotTrump) shouldEqual false
  }

  "isCardValidToMakeAttack" should "not validate card to make attack with " +
    "if player has required suit but card is of different suit" in {
    isCardValidToMakeAttack(
      card3,
      playerWithTrump,
      strongestCardNotTrump,
      cardToHitNotTrump) shouldEqual false
  }

  "isCardValidToMakeAttack" should "not validate card to make attack with " +
    "if player has card of required suit and  of stronger rank than strongest card " +
    "and card is of required suit but of weaker rank" in {
    isCardValidToMakeAttack(
      card2,
      playerWithTrump,
      strongestCardNotTrump,
      cardToHitNotTrump) shouldEqual false
  }

  "isCardValidToMakeAttack" should "validate card to make attack with " +
    "if card is of required suit and of stronger rank than strongest card" in {
    isCardValidToMakeAttack(
      card4,
      playerWithTrump,
      strongestCardNotTrump,
      cardToHitNotTrump) shouldEqual true
  }

  "isCardValidToMakeAttack" should "not validate card to make attack with " +
    "if player has card of required suit but card is trump" in {
    isCardValidToMakeAttack(
      trump,
      playerWithTrump,
      strongestCardNotTrump,
      cardToHitNotTrump) shouldEqual false
  }

  "isCardValidToMakeAttack" should "validate any card to make attack with " +
    "if player does not have card of required suit and trump" in {
    isCardValidToMakeAttack(
      card1,
      playerWithoutTrump,
      strongestCardTrump,
      cardToHitTrump) shouldEqual true
    isCardValidToMakeAttack(
      card2,
      playerWithoutTrump,
      strongestCardTrump,
      cardToHitTrump) shouldEqual true
  }

  "isCardValidToMakeAttack" should "validate trump card to make attack with " +
    "if player does not have card of required suit" in {
    isCardValidToMakeAttack(
      trump,
      playerWithTrump,
      strongestCardNotTrumpUnique,
      cardToHitNotTrumpUnique) shouldEqual true
  }

  "isCardValidToMakeAttack" should "not validate card to make attack with " +
    "if player does not have card of required suit and has trump but card is not trump" in {
    isCardValidToMakeAttack(
      card1,
      playerWithTrump,
      strongestCardNotTrumpUnique,
      cardToHitNotTrumpUnique) shouldEqual false
  }

  "isCardValidToMakeAttack" should "not validate card to make attack with " +
    "if player does not have card of required suit, " +
    "strongest card is trump and player has a trump of stronger rank " +
    "than strongest card but card is a trump of weaker rank" in {
    isCardValidToMakeAttack(
      trump,
      playerWithTrumps,
      strongestCardTrump,
      cardToHitNotTrumpUnique) shouldEqual false
  }

  "isCardValidToMakeAttack" should "validate card to make attack with " +
    "if player does not have card of required suit, " +
    "strongest card is trump and card is a trump of stronger rank " in {
    isCardValidToMakeAttack(
      trumpHigh,
      playerWithTrumps,
      strongestCardTrump,
      cardToHitNotTrumpUnique) shouldEqual true
  }

  "isCardStronger" should "return true if card is a trump and strongest card is not a trump" in {
    isCardStronger(trump, strongestCardNotTrump.value) shouldEqual true
  }

  "isCardStronger" should "return false if card is not a trump and strongest card is a trump" in {
    isCardStronger(card1, strongestCardNotTrump.value) shouldEqual false
  }

  "isCardStronger" should "return true if strongest card is a trump card " +
    "but card is a trump of higher rank" in {
    isCardStronger(trumpHigh, strongestCardTrump.value) shouldEqual true
  }

  "isCardStronger" should "return false if card is a trump " +
    "but strongest card is a trump of higher rank" in {
    isCardStronger(trump, strongestCardTrump.value) shouldEqual false
  }

  "isCardStronger" should "return true if card is of the same suit as strongest card " +
    "and card is of higher rank" in {
    isCardStronger(card4, strongestCardNotTrump.value) shouldEqual true
  }

  "isCardStronger" should "return false if card is of the same suit as strongest card " +
    "and card is of weaker rank" in {
    isCardStronger(card2, strongestCardNotTrump.value) shouldEqual false
  }


}