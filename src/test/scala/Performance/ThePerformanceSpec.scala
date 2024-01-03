package Performance

import Packet.CardValue.cardValues
import Packet.ThePerformance.*
import Support.DeckGens.shuffledDecks
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class ThePerformanceSpec extends ScalaCheckSuite:

  test("all working hands should have eleven unique cards") {
    forAll(shuffledDecks) { deck =>
      val elevenUniqueCardsValues =
        dealTheWorkingHands(deck)
          .elevenUniqueCards
          .map(cards => cards.value)
          .toSet

      assert(elevenUniqueCardsValues.size == 11)
    }
  }

  test("the remaining pair shouldn't be in the unique eleven") {
    forAll(shuffledDecks) { deck =>
      val workingHands = dealTheWorkingHands(deck)
      val uniqueCardsValues = elevenUniqueCardsValues(workingHands)
      val remainingPair = theRemainingPair(workingHands, cardValues)

      assert(!uniqueCardsValues.contains(remainingPair.card1))
      assert(!uniqueCardsValues.contains(remainingPair.card2))

    }
  }