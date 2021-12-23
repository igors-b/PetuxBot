package domain.cardcontainers

import com.petuxbot.domain.Card
import com.petuxbot.domain.Rank.King
import com.petuxbot.domain.Suit.Diamonds
import com.petuxbot.domain.cardcontainers.DiscardPile
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DiscardPileSpec extends AnyFlatSpec with Matchers {
  val emptyDiscardPile: DiscardPile = DiscardPile.Empty
  val card: Card = Card(King, Diamonds)
  val cards = List(card)

  "addCards" should "return new discard pile with added cards" in {
    emptyDiscardPile.cards.isEmpty shouldEqual true
    emptyDiscardPile.addCards(cards).cards.contains(card) shouldEqual true
  }
}
