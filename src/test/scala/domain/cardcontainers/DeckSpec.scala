package domain.cardcontainers

import com.petuxbot.domain.Card
import com.petuxbot.domain.Rank._
import com.petuxbot.domain.Suit._
import com.petuxbot.domain.cardcontainers.{Deck, Hand}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DeckSpec extends AnyFlatSpec with Matchers{
  val emptyHands: List[Hand] = List.empty[Hand]
  val hands: List[Hand] = List(
    Hand.Empty,
    Hand.Empty
  )
  val emptyDeck: Deck = Deck.Empty
  val deck: Deck = Deck(List(
    Card(Ace, Spades, isTrump = true),
    Card(Ace, Diamonds),
    Card(Ace, Clubs),
    Card(Ace, Hearts),
    Card(King, Spades, isTrump = true),
    Card(King, Diamonds),
    Card(King, Clubs),
    Card(King, Hearts),
    Card(Queen, Spades, isTrump = true),
    Card(Queen, Diamonds),
    Card(Queen, Clubs),
    Card(Queen, Hearts)
  ))

  "deal" should "return None if list of hands is empty" in {
    deck.deal(emptyHands) shouldEqual None
  }

  "deal" should "return Some of empty deck and hands if deck is empty" in {
    emptyDeck.deal(hands) shouldEqual Some(emptyDeck, hands)
  }

  "deal" should "return Some of new deck without dealt cards and list of dealt hands" in {
    val dealingResult = deck.deal(hands)
    val result =
    dealingResult match {
      case Some(value) =>
        val (newDeck, dealtHands) = value
        (deck.cards.size == 12) &&
          (newDeck.cards.size == 2) &&
          !dealtHands.map(_.cards.size).exists(_ != 5)

      case None        => false
    }
    result shouldEqual true
  }


}
