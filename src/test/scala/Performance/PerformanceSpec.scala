package Performance

import Packet.ShuffledDeck
import Packet.Performance.*
import Support.DeckGens.{shuffledDecks, workingHandsAndPairs, nonWorkingHandsAndPairs}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Prop.forAll

class PerformanceSpec extends CatsEffectSuite with ScalaCheckEffectSuite:

  test("all working hands should have eleven unique cards") {
    forAll(shuffledDecks) { deck =>
      val workingHands = dealTheWorkingHands(deck)
      val elevenUniqueCardsValues =
        workingHands
          .elevenUniqueCards
          .map(cards => cards.value)
          .toSet
      assert(elevenUniqueCardsValues.size == 11)
    }
  }

  test("the remaining pair shouldn't be in the unique eleven") {
    forAll(workingHandsAndPairs) { handAndPair =>

      val uniqueCardsValues = elevenUniqueCardsValues(handAndPair.hands)
      val remainingPair = handAndPair.pair

      assert(!uniqueCardsValues.contains(remainingPair.cards.head))
      assert(!uniqueCardsValues.contains(remainingPair.cards.last))

    }
  }


  test("result of trick should identify when the remaining pair is in the remainder of deck") {
    forAll(workingHandsAndPairs) { handsAndPair =>

      val result: ShuffledDeck.ResultOfTrick = 
        resultOfTrick(Nil, handsAndPair.pair, handsAndPair.hands.remainderOfDeck)

      assert(result.successes >= 1)
    }
  }

  test("result of trick should identify when the remaining pair is not in the remainder of deck") {
    forAll(nonWorkingHandsAndPairs) { handsAndPair =>

      val result: ShuffledDeck.ResultOfTrick =
        resultOfTrick(Nil, handsAndPair.pair, handsAndPair.hands.remainderOfDeck)
      
      assert(result.successes == 0)
    }
  }

  