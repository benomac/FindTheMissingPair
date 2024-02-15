package Support

import Packet.CardValue.cardValues
import Packet.Performance.{PairToLookFor, WorkingHands, dealTheWorkingHands, elevenUniqueCardsValues, theRemainingPair}
import Packet.{CardValue, Suit}
import Packet.ShuffledDeck.{Card, ShuffledDeck}
import org.scalacheck.Gen
import org.scalacheck.Gen.*

import scala.util.Random

object DeckGens:
  def suits: Gen[List[Suit]] = Gen.const(Suit.suits)

  def values: Gen[List[CardValue]] = Gen.const(CardValue.cardValues)

  def shuffledDecks: Gen[ShuffledDeck] = {
    for {
      v <- values
      s <- suits
      deck = v.flatMap(value => s.map(suit => Card(value, suit)))
    } yield ShuffledDeck(Random.shuffle(deck))
  }

  def workingHands: Gen[WorkingHands] =
    shuffledDecks.flatMap(d => dealTheWorkingHands(d))

  def remainingPairs(workingHands: WorkingHands): Gen[PairToLookFor] =
    for
      workingHand <- workingHands
      allValues <- values
    yield theRemainingPair(workingHand, allValues)

  case class HandsAndPairs(hands: WorkingHands, pair: PairToLookFor)

  def handsAndPairs: Gen[HandsAndPairs] =
    for
      hand <- workingHands
      pair <- remainingPairs(hand)
    yield HandsAndPairs(hand, pair)

  def workingHandsAndPairs: Gen[HandsAndPairs] =
    handsAndPairs.retryUntil(handPair => (
      handPair.hands.remainderOfDeck.map(_.value).containsSlice(handPair.pair.cards)
        || handPair.hands.remainderOfDeck.map(_.value).containsSlice(handPair.pair.cards.reverse))
      && (handPair.hands.remainderOfDeck.map(_.value).containsSlice(handPair.pair.cards)
      && handPair.hands.remainderOfDeck.map(_.value).containsSlice(handPair.pair.cards.reverse)),
      1000
    )

  def nonWorkingHandsAndPairs: Gen[HandsAndPairs] =
    handsAndPairs.retryUntil(handPair => !(
      handPair.hands.remainderOfDeck.map(_.value).containsSlice(handPair.pair.cards)
        || handPair.hands.remainderOfDeck.map(_.value).containsSlice(handPair.pair.cards.reverse))
      && !(handPair.hands.remainderOfDeck.map(_.value).containsSlice(handPair.pair.cards)
      && handPair.hands.remainderOfDeck.map(_.value).containsSlice(handPair.pair.cards.reverse)),
      1000
    )










