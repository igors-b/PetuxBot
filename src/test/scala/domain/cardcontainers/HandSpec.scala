package domain.cardcontainers

import com.petuxbot.domain.Card
import com.petuxbot.domain.Rank.King
import com.petuxbot.domain.Suit.Diamonds
import com.petuxbot.domain.cardcontainers.Hand
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HandSpec extends AnyFlatSpec with Matchers {

  val emptyHand: Hand = Hand.Empty
  val card: Card = Card(King, Diamonds)
  val cards = List(card)

  "addCards" should "return new hand with added cards" in {
    emptyHand.cards.isEmpty shouldEqual true
    emptyHand.addCards(cards).cards.contains(card) shouldEqual true
  }

  "removeCard" should "return new hand with removed card" in {
    Hand(List(card)).removeCard(card) shouldEqual emptyHand
  }

  "removeCards" should "return new hand with removed cards" in {
    Hand(List(card)).removeCards(List(card)) shouldEqual emptyHand
  }

}
