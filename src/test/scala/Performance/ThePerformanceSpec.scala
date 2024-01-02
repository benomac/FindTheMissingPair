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
        dealTheHands(deck)
          .elevenUniqueCards
          .map(cards => cards.value)
          .toSet

      assert(elevenUniqueCardsValues.size == 11)
    }
  }

  test("the remaining pair shouldn't be in the unique eleven") {
    forAll(shuffledDecks) { deck =>
      val dealtHands = dealTheHands(deck)
      val elevenUniqueCardsValues =
        dealtHands
          .elevenUniqueCards
          .map(cards => cards.value)

      val remainingPair = theRemainingPair(dealtHands, cardValues)

      assert(!elevenUniqueCardsValues.contains(remainingPair.card1))
      assert(!elevenUniqueCardsValues.contains(remainingPair.card2))

    }
  }