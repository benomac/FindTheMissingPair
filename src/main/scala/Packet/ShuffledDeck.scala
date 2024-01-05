package Packet

import Packet.CardValue.cardValues
import Packet.Suit.suits
import cats.effect.IO
import cats.implicits.*
import traits.Deck

import scala.util.Random


object ShuffledDeck extends Deck[IO]:

  case class Card(value: CardValue, suit: Suit):
    def asString = s"$value of $suit"
    def asFoundCard = s"${value.toString.toUpperCase} of $suit"

  case class ShuffledDeck(cards: List[Card])

  override def shuffledDeck: IO[ShuffledDeck] =
    val deck = for {
      card <- cardValues
      suit <- suits
    } yield Card(card, suit)
    IO(ShuffledDeck(Random.shuffle(deck)))

  def shuffledDeckOption: Option[ShuffledDeck] =
    val deck = for {
      card <- cardValues
      suit <- suits
    } yield Card(card, suit)
    Option(ShuffledDeck(Random.shuffle(deck)))
