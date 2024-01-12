import Packet.CardValue
import Packet.Performance.{dealTheWorkingHands, formatDeck, resultOfTrick, theRemainingPair}
import Packet.ShuffledDeck.{Card, shuffledDeck}
import cats.effect.IO

def capitalizeSublistOccurrences(mainList: List[Card], sublist: List[Card]): List[Card] = {
  val sublistLength = sublist.length

  mainList
    .foldLeft((List.empty[Card], List.empty[Card])) {
      case ((acc, buffer), element) =>
        if (buffer.length < sublistLength && element == sublist(buffer.length)) {
          (acc, buffer :+ element)
        } else {
          if (buffer == sublist) {
            (acc ++ sublist.map(_.copy(isInAPair = true)) ++ List(element), List.empty[Card])
          } else {
            (acc :+ element, List.empty[Card])
          }
        }
    }
    ._1
}

//val res: IO[(Int, List[String])] =
//  for
//    deck <- shuffledDeck
//    workingHands = dealTheWorkingHands(deck)
//    remainingPair = theRemainingPair(workingHands, CardValue.cardValues)
//    _ = println(remainingPair)
//    result = resultOfTrick(Nil, remainingPair, workingHands.remainderOfDeck)
//    formatted = formatDeck(result.deck)
//  yield (result.successes, formatted)
//
//val mainList = List(1, 2, 3, 4, 2, 3, 4, 3, 5, 6, 7)
//val sublistToFind = List(2, 3)
//
//val result = capitalizeSublistOccurrences(mainList, sublistToFind)
//println(s"Original list with occurrences capitalized: $result")

"AAA".forall(_.isLower)
