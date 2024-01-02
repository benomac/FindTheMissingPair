package Packet

import Packet.ShuffledDeck.{Card, ShuffledDeck, shuffledDeck}

import scala.annotation.tailrec

object ThePerformance:

  case class WorkingHands(remainderOfDeck: List[Card], elevenUniqueCards: List[Card])
  case class PairToLookFor(card1: Card, card2: Card)

  @tailrec
  def dealTheHands(deck: ShuffledDeck, elevenUniqueCards: List[Card] = Nil): WorkingHands = {
    (deck.cards, elevenUniqueCards.size == 11) match {
      case (::(head, next), false) =>
        if (elevenUniqueCards.forall(a => head.value != a.value)) {
          dealTheHands(ShuffledDeck(next), elevenUniqueCards :+ head)
        }
        else {
          dealTheHands(ShuffledDeck(next), elevenUniqueCards)
        }
      case _ => WorkingHands(deck.cards, elevenUniqueCards)
    }
  }

  def main(args: Array[String]): Unit = {
  }