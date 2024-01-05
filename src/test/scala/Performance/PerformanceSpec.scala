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
    forAllF(shuffledDecks) { deck =>
      for
        workingHands <- dealTheWorkingHands(deck)
        elevenUniqueCardsValues =
          workingHands
            .elevenUniqueCards
            .map(cards => cards.value)
            .toSet
        _ <- IO(assert(elevenUniqueCardsValues.size == 11))
      yield ()
    }
  }

  test("the remaining pair shouldn't be in the unique eleven") {
    forAllF(shuffledDecks) { deck =>
      for
        workingHands      <- dealTheWorkingHands(deck)
        uniqueCardsValues <- elevenUniqueCardsValues(workingHands)
        remainingPair     <- theRemainingPair(workingHands, cardValues)

        _ <- IO(assert(!uniqueCardsValues.contains(remainingPair.card1)))
        _ <- IO(assert(!uniqueCardsValues.contains(remainingPair.card2)))
      yield ()
    }
  }
  