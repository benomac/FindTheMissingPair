package Packet

import Packet.ShuffledDeck.{Card, ShuffledDeck, shuffledDeck}
import cats.effect.{ExitCode, IO, IOApp}

import scala.annotation.tailrec

object Performance extends IOApp :

  case class WorkingHands(remainderOfDeck: List[Card], elevenUniqueCards: List[Card])

  case class PairToLookFor(card1: CardValue, card2: CardValue)

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

  private def theRemainingPair(workingHands: WorkingHands, allCardValues: List[CardValue]): PairToLookFor = {
    val elevenValues = elevenUniqueCardsValues(workingHands)
    val pairValuesToFind = pairValues(allCardValues, elevenValues, Nil)
    PairToLookFor(pairValuesToFind.head, pairValuesToFind.last)
  }

  @tailrec
  def isThePairTogetherInTheRemainder(checkedCards: List[String], pair: PairToLookFor, remainderOfDeck: List[Card], acc: Int = 0): (Int, List[String]) = {
    remainderOfDeck match {
      case _ if remainderOfDeck.size < 2 => (acc, checkedCards ++ remainderOfDeck.map(_.asString))
      case ::(head, next) if List(head.value, next.head.value) == List(pair.card1, pair.card2)
      => isThePairTogetherInTheRemainder(checkedCards ++ List(head.asFoundCard, next.head.asFoundCard), pair, next.drop(1), acc + 1)
      case ::(head, next) if List(head.value, next.head.value) == List(pair.card2, pair.card1)
      => isThePairTogetherInTheRemainder(checkedCards ++ List(next.head.asFoundCard, head.asFoundCard), pair, next.drop(1), acc + 1)
      case ::(head, next) => isThePairTogetherInTheRemainder(checkedCards :+ head.asString, pair, next, acc)
      case _ => (acc, checkedCards ++ remainderOfDeck.map(_.asString))
    }
  }

  val res: IO[(Int, List[String])] = for
    deck <- shuffledDeck
    workingHands = dealTheWorkingHands(deck)
    remainingPair = theRemainingPair(workingHands, CardValue.cardValues)
    _ = println(remainingPair)
    result = isThePairTogetherInTheRemainder(Nil, remainingPair, workingHands.remainderOfDeck)
  yield result


  override def run(args: List[String]): IO[ExitCode] =
    res.flatMap(r => IO(println(s"result: $r"))).as(ExitCode.Success)

