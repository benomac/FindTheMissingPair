package traits

import Packet.CardValue
import Packet.ShuffledDeck.{Card, ShuffledDeck}
import Packet.Performance.{PairToLookFor, WorkingHands}

trait Performance[F[_]]:
  
  def dealTheWorkingHands(deck: ShuffledDeck, elevenUniqueCards: List[Card] = Nil): F[WorkingHands]
  
  def elevenUniqueCardsValues(workingHands: WorkingHands): F[List[CardValue]]
  
  def theRemainingPair(workingHands: WorkingHands, allCardValues: List[CardValue]): F[PairToLookFor]
  
  def isThePairTogetherInTheRemainder(checkedCards: List[String], pair: PairToLookFor, remainderOfDeck: List[Card], acc: Int = 0): F[(Int, List[String])]
