package Performance

import Packet.CardValue.cardValues
import Packet.Performance.*
import Support.DeckGens.shuffledDecks
import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite, ScalaCheckSuite}
import org.scalacheck.Prop.forAll
import org.scalacheck.effect.PropF.forAllF

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
    forAll(shuffledDecks) { deck =>

      val workingHands      = dealTheWorkingHands(deck)
      val uniqueCardsValues = elevenUniqueCardsValues(workingHands)
      val remainingPair     = theRemainingPair(workingHands, cardValues)

      assert(!uniqueCardsValues.contains(remainingPair.card1))
      assert(!uniqueCardsValues.contains(remainingPair.card2))

    }
  }
  