package Packet

import Packet.ShuffledDeck.{Card, ShuffledDeck}
import cats.effect.IO
import traits.Performance

import scala.annotation.tailrec

object Performance extends Performance[IO]:

  case class WorkingHands(remainderOfDeck: List[Card], elevenUniqueCards: List[Card])

  case class PairToLookFor(card1: CardValue, card2: CardValue)

  @tailrec
  override def dealTheWorkingHands(deck: ShuffledDeck, elevenUniqueCards: List[Card] = Nil): IO[WorkingHands] = {
    (deck.cards, elevenUniqueCards.size == 11) match {
      case (::(head, next), false) =>
        if (elevenUniqueCards.forall(a => head.value != a.value)) {
          dealTheWorkingHands(ShuffledDeck(next), elevenUniqueCards :+ head)
        }
        else {
          dealTheWorkingHands(ShuffledDeck(next), elevenUniqueCards)
        }
      case _ => IO(WorkingHands(deck.cards, elevenUniqueCards))
    }
  }

  override def elevenUniqueCardsValues(workingHands: WorkingHands): IO[List[CardValue]] =
    IO(workingHands
      .elevenUniqueCards
      .flatMap(c => List(c.value)))

  private def pairValues(cardValues: List[CardValue], elevenValues: List[CardValue], pair: List[CardValue]): IO[List[CardValue]] =
    IO(cardValues
      .flatMap(cv => if (elevenValues.contains(cv)) Nil else pair :+ cv))

  override def theRemainingPair(workingHands: WorkingHands, allCardValues: List[CardValue]): IO[PairToLookFor] = {
    for
      elevenValues <- elevenUniqueCardsValues(workingHands)
      pairValuesToFind <- pairValues(allCardValues, elevenValues, Nil)
    yield PairToLookFor(pairValuesToFind.head, pairValuesToFind.last)
  }

  @tailrec
  def isThePairTogetherInTheRemainder(checkedCards: List[String], pair: PairToLookFor, remainderOfDeck: List[Card], acc: Int = 0): IO[(Int, List[String])] = {
    remainderOfDeck match {
      case _ if remainderOfDeck.size < 2 => IO((acc, checkedCards :+ remainderOfDeck.last.asString))
      case ::(head, next) if List(head.value, next.head.value) == List(pair.card1, pair.card2)
      => isThePairTogetherInTheRemainder(checkedCards ++ List(head.asFoundCard, next.head.asFoundCard), pair, next.drop(1), acc + 1)
      case ::(head, next) if List(head.value, next.head.value) == List(pair.card2, pair.card1)
      => isThePairTogetherInTheRemainder(checkedCards ++ List(next.head.asFoundCard, head.asFoundCard), pair, next.drop(1), acc + 1)
      case ::(head, next) => isThePairTogetherInTheRemainder(checkedCards :+ head.asString, pair, next, acc)
      case _ => IO((acc, checkedCards :+ remainderOfDeck.last.asString))
    }
  }