package services

import cats.Id
import com.petuxbot.domain.Card
import com.petuxbot.domain.Rank.Ranks
import com.petuxbot.domain.Suit.{Spades, Suits}
import com.petuxbot.domain.cardcontainers.Deck
import com.petuxbot.services.{DeckService, ShuffleService}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.BuildFrom
import scala.collection.mutable.ArrayBuffer

class DeckServiceSpec extends AnyWordSpec with Matchers {
  val allCards: List[Card] =
    Ranks.flatMap(rank => Suits.map(suit => Card(rank, suit))).toList

  val cardsWithTrumps: List[Card] = allCards.map(card => if (card.suit == Spades) card.copy(isTrump = true) else card)
  val deck: Deck = Deck(cardsWithTrumps)

  "DeckService" should {

    "returned shuffled deck with trumps" in new Scope {
      deckService.of shouldBe deck
    }
  }

  private trait Scope {

    val shuffleService: ShuffleService[Id] = new ShuffleService[Id] { // this is a stub
      def apply[T, C](xs: IterableOnce[T])(implicit bf: BuildFrom[xs.type, T, C]): Id[C] =
        (bf.newBuilder(xs) ++= (new ArrayBuffer[T] ++= xs)).result() // returns collection as is
    }

    val deckService: DeckService[Id] = DeckService(shuffleService)
  }

}
