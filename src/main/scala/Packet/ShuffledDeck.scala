package Packet

import Packet.CardValue.cardValues
import Packet.Suit.suits
import cats.implicits._

import scala.util.Random

object ShuffledDeck:

  case class Card(value: CardValue, suit: Suit)

  case class ShuffledDeck(cards: List[Card])

  def shuffledDeck: ShuffledDeck =
    val deck = for {
      card <- cardValues
      suit <- suits
    } yield Card(card, suit)
    ShuffledDeck(Random.shuffle(deck))

  
