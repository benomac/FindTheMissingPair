package Packet

import Packet.CardValue.cardValues
import Packet.Suit.suits
import cats.effect.IO
import cats.implicits.*

import scala.util.Random


object ShuffledDeck:

  case class Card(value: CardValue, suit: Suit, isInAPair: Boolean = false):
    def asString = s"$value of $suit"
    def asFoundCard = s"${value.toString.toUpperCase} OF ${suit.toString.toUpperCase}"

  case class ResultOfTrick(successes: Int, deck: List[Card])
  case class ShuffledDeck(cards: List[Card])

  def shuffleDeck: IO[ShuffledDeck] =
    val deck = for {
      card <- cardValues
      suit <- suits
    } yield Card(card, suit)
    IO(ShuffledDeck(Random.shuffle(deck)))

  def renderDeck(deck: List[Card]): List[String] = {
    deck
      .map(c => if (c.isInAPair) c.asFoundCard else c.asString)
  }


