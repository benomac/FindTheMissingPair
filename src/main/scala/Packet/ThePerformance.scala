package Packet

import Packet.ShuffledDeck.{Card, ShuffledDeck, shuffledDeck}

import scala.annotation.tailrec

object ThePerformance:

  case class WorkingHands(remainderOfDeck: List[Card], elevenUniqueCards: List[Card])

  case class PairToLookFor(card1: CardValue, card2: CardValue)

  @tailrec
  def dealTheWorkingHands(deck: ShuffledDeck, elevenUniqueCards: List[Card] = Nil): WorkingHands = {
    (deck.cards, elevenUniqueCards.size == 11) match {
      case (::(head, next), false) =>
        if (elevenUniqueCards.forall(a => head.value != a.value)) {
          dealTheWorkingHands(ShuffledDeck(next), elevenUniqueCards :+ head)
        }
        else {
          dealTheWorkingHands(ShuffledDeck(next), elevenUniqueCards)
        }
      case _ => WorkingHands(deck.cards, elevenUniqueCards)
    }
  }

  def elevenUniqueCardsValues(workingHands: WorkingHands): List[CardValue] =
    workingHands
      .elevenUniqueCards
      .flatMap(c => List(c.value))

  def pairValues(cardValues: List[CardValue], elevenValues: List[CardValue], pair: List[CardValue]): List[CardValue] =
    cardValues
      .flatMap(cv => if (elevenValues.contains(cv)) Nil else pair :+ cv)

  def theRemainingPair(workingHands: WorkingHands, cardValues: List[CardValue]): PairToLookFor = {
    val elevenValues: List[CardValue] = elevenUniqueCardsValues(workingHands)
    val pairValuesToFind: List[CardValue] =
      pairValues(cardValues, elevenValues, Nil)
    PairToLookFor(pairValuesToFind.head, pairValuesToFind.last)
  }

  def main(args: Array[String]): Unit = {
  }