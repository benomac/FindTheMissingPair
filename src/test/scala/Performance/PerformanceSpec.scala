package Performance

import Packet.{CardValue, ShuffledDeck}
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

      val workingHands = dealTheWorkingHands(deck)
      val uniqueCardsValues = elevenUniqueCardsValues(workingHands)
      val remainingPair = theRemainingPair(workingHands, cardValues)

      assert(!uniqueCardsValues.contains(remainingPair.cards.head))
      assert(!uniqueCardsValues.contains(remainingPair.cards.last))

    }
  }

  test("result of trick should identify remaining pair, in remainder of deck") {
    forAll(shuffledDecks) {
      deck =>
        val workingHands: WorkingHands = dealTheWorkingHands(deck)
        val remainingPair: PairToLookFor = theRemainingPair(workingHands, CardValue.cardValues)
        //        val pairContained: Boolean = containsThePair(remainingPair, workingHands.remainderOfDeck)
        val result: ShuffledDeck.ResultOfTrick = resultOfTrick(Nil, remainingPair, workingHands.remainderOfDeck)
        val pairsInDeckFound: List[CardValue] = result.deck.filter(_.isInAPair).map(c => c.value)

        assert(result.deck.count(_.isInAPair) == result.successes * 2)
        println(result.deck.filter(_.isInAPair).map(_.value) )
        println(remainingPair.cards)
        if (workingHands.remainderOfDeck.map(_.value).containsSlice(remainingPair.cards))
          assert{
            println("pairs")
            result.deck.map(_.value).containsSlice(remainingPair.cards)
          }
          assert(result.deck.filter(_.isInAPair).map(_.value).toSet == remainingPair.cards.toSet)
        else if (workingHands.remainderOfDeck.map(_.value).containsSlice(remainingPair.cards.reverse))
          assert{
            println("pairs")
            result.deck.map(_.value).containsSlice(remainingPair.cards.reverse)
          }
          assert(result.deck.filter(_.isInAPair).map(_.value).toSet == remainingPair.cards.toSet)
        else
          assert{
            println("no pairs")
            1 == 1
          }

    }
  }

 
  