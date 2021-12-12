package com.petuxbot

import com.petuxbot.Request._
import com.petuxbot.domain.Rank.Ranks
import com.petuxbot.domain.Suit.Suits
import com.petuxbot.domain._
import io.circe.generic.semiauto.deriveCodec
import io.circe.generic.auto._
import io.circe.{Codec, Decoder, Encoder}
import io.circe.syntax._
import cats.syntax.functor._
import com.petuxbot.domain.cardcontainers.{Board, Deck, Hand}

object Codecs {
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

  implicit val commandEncoder: Encoder[Request] = Encoder.instance {
    case StartNewRound            => Encoder.encodeString("deal")
    case cc @ ChangeCards(_)      => cc.asJson
    case mt @ MakeTurnWithCard(_) => mt.asJson
  }

  implicit val commandDecoder: Decoder[Request] =
    List[Decoder[Request]](
      Decoder.decodeString.emap(str => if (str == "deal") Right(StartNewRound) else Left("wrong request")).widen,
      Decoder[ChangeCards].widen,
      Decoder[MakeTurnWithCard].widen
    ).reduce(_ or _)
}
