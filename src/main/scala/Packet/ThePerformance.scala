package Packet

import Packet.CardValue.cardValues
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

  @tailrec
  def isThePairTogetherInTheRemainder(checkedCards: List[String], pair: PairToLookFor, remainderOfDeck: List[Card], acc: Int = 0): (Int, List[String]) = {
    remainderOfDeck match {
      case _ if remainderOfDeck.size < 2 => (acc, checkedCards :+ remainderOfDeck.last.asString)
      case ::(head, next) if List(head.value, next.head.value) == List(pair.card1, pair.card2)
      => isThePairTogetherInTheRemainder(checkedCards ++ List(head.asFoundCard, next.head.asFoundCard), pair, next.drop(1), acc + 1)
      case ::(head, next) if List(head.value, next.head.value) == List(pair.card2, pair.card1)
      => isThePairTogetherInTheRemainder(checkedCards ++ List(next.head.asFoundCard, head.asFoundCard), pair, next.drop(1), acc + 1)
      case ::(head, next) => isThePairTogetherInTheRemainder(checkedCards :+ head.asString, pair, next, acc)
      case _ => (acc, checkedCards :+ remainderOfDeck.last.asString)
    }
  }

  def main(args: Array[String]): Unit = {
    val workingDeck = shuffledDeck
    val workingHands = dealTheWorkingHands(workingDeck)
    val pairToFind = theRemainingPair(workingHands, cardValues)
    val result = isThePairTogetherInTheRemainder(Nil, pairToFind, workingHands.remainderOfDeck)
    println(pairToFind)
    println(result)
  }