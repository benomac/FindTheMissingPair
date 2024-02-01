package Packet

import Packet.ShuffledDeck.{Card, ResultOfTrick, ShuffledDeck, renderDeck}
import Packet.Suit.*

import scala.annotation.tailrec

object Performance:

  case class WorkingHands(remainderOfDeck: List[Card], elevenUniqueCards: List[Card])

  case class PairToLookFor(cards: List[CardValue])

  @tailrec // none of these need to to be IO as they are referentially transparent 
  // (given the same input, they will always give the same output)
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

  private def pairValues(cardValues: List[CardValue], elevenValues: List[CardValue], pair: List[CardValue]): List[CardValue] =
    cardValues
      .flatMap(cv => if (elevenValues.contains(cv)) Nil else pair :+ cv)

  def theRemainingPair(workingHands: WorkingHands, allCardValues: List[CardValue]): PairToLookFor = {
    val elevenValues = elevenUniqueCardsValues(workingHands)
    val pairValuesToFind = pairValues(allCardValues, elevenValues, Nil)
    PairToLookFor(pairValuesToFind)
  }

  @tailrec
  def resultOfTrick(checkedCards: List[Card], pair: PairToLookFor, remainderOfDeck: List[Card], acc: Int = 0): ResultOfTrick =
    if (remainderOfDeck.size < 2) ResultOfTrick(acc, checkedCards ++ remainderOfDeck)
    else
      remainderOfDeck match
        case ::(head, next) if List(head.value, next.head.value) == pair.cards ||
          List(head.value, next.head.value) == pair.cards.reverse
        => resultOfTrick(checkedCards ++ List(head.copy(isInAPair = true),
          next.head.copy(isInAPair = true)), pair,
          next.drop(1), acc + 1)
        case ::(head, next) => resultOfTrick(checkedCards :+ head, pair, next, acc)
        case _ => ResultOfTrick(acc, checkedCards ++ remainderOfDeck)
  
  def containsThePair(pair: PairToLookFor, remainderOfDeck: List[Card], acc: Int = 0): Boolean = {
    remainderOfDeck.map(_.value).containsSlice(pair.cards) ||
      remainderOfDeck.map(_.value).containsSlice(pair.cards.reverse)
  }
  


