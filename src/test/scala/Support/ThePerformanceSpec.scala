package Support

import Packet.ThePerformance.*
import Packet.ThePerformance.dealTheHands
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