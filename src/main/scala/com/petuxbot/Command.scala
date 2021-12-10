package com.petuxbot

import com.petuxbot.Command._
import com.petuxbot.domain.Rank.Ranks
import com.petuxbot.domain.Suit.Suits
import com.petuxbot.domain._
import io.circe.generic.semiauto.deriveCodec
import io.circe.generic.auto._
import io.circe.{Codec, Decoder, Encoder}
import io.circe.syntax._
import cats.syntax.functor._
import com.petuxbot.domain.cardContainers.{Board, Deck, Hand}


sealed trait Command
object Command {
  case object StartNewRound extends Command
  final case class ChangeCards(cards: List[Card]) extends Command
  final case class MakeTurnWithCard(card: Card) extends Command
// case object WrongCommand extends Command
}

object ImplicitCodecs {

  implicit val suitDecoder: Decoder[Suit] =
    Decoder.decodeString.emap(str =>
    Suits.find(_.toString == str).toRight("invalid suit")
  )

  implicit val suitEncoder: Encoder[Suit] =
    Encoder.encodeString.contramap(_.toString)

  implicit val rankDecoder: Decoder[Rank] = Decoder.decodeString.emap(string =>
    Ranks.find(_.toString == string).toRight("invalid rank")
  )

  implicit val rankEncoder: Encoder[Rank] =
    Encoder.encodeString.contramap(_.toString)

  implicit val cardCodec: Codec[Card] = deriveCodec[Card]

  implicit val deckCodec: Codec[Deck] = deriveCodec[Deck]

  implicit val handCodec: Codec[Hand] = deriveCodec[Hand]

  implicit val boardCodec: Codec[Board] = deriveCodec[Board]

  implicit val responseCodec: Codec[Response] = deriveCodec[Response]

  implicit val commandEncoder: Encoder[Command] = Encoder.instance {
    case StartNewRound            => Encoder.encodeString("Deal")
//    case WrongCommand             => Encoder.encodeString("WrongCommand")
    case cc @ ChangeCards(_)      => cc.asJson
    case mt @ MakeTurnWithCard(_) => mt.asJson
  }

  implicit val commandDecoder: Decoder[Command] =
    List[Decoder[Command]](
      Decoder.decodeString.emap(str => if (str == "Deal") Right(StartNewRound) else Left("wrong command")).widen,
//      Decoder.decodeString.emap(str => if (str == "WrongCommand") Right(WrongCommand) else Left("wrong command")).widen,
      Decoder[ChangeCards].widen,
      Decoder[MakeTurnWithCard].widen
    ).reduce(_ or _)
}
