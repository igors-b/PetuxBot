package com.petuxbot

import com.petuxbot.Command.{AddPlayers, DealCard, StartGame, WrongCommand}
import com.petuxbot.domain.Rank.Ranks
import com.petuxbot.domain.Suit.Suits
import com.petuxbot.domain._
import io.circe.generic.semiauto.deriveCodec
import io.circe.generic.auto._
import io.circe.{Codec, Decoder, Encoder}
import io.circe.syntax._
import cats.syntax.functor._


sealed trait Command
object Command {
  case object StartGame extends Command
  case class AddPlayers(players: List[Player]) extends Command
  case object DealCard extends Command
  case object WrongCommand extends Command
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
    case StartGame           => Encoder.encodeString("StartGame")
    case DealCard            => Encoder.encodeString("DealCard")
    case WrongCommand        => Encoder.encodeString("WrongCommand")
    case ap @ AddPlayers(_)  => ap.asJson
  }

  implicit val commandDecoder: Decoder[Command] =
    List[Decoder[Command]](
      Decoder.decodeString.emap(str => if (str == "StartGame") Right(StartGame) else Left("wrong command")).widen,
      Decoder.decodeString.emap(str => if (str == "DealCard") Right(DealCard) else Left("wrong command")).widen,
      Decoder.decodeString.emap(str => if (str == "WrongCommand") Right(DealCard) else Left("wrong command")).widen,
      Decoder[AddPlayers].widen
    ).reduce(_ or _)
}
