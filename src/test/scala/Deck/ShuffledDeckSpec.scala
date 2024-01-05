package Deck

import Packet.ShuffledDeck.{shuffledDeck, shuffledDeckOption}
import Support.DeckGens.{cards, shuffledDecks}
import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.effect.PropF.forAllF

class ShuffledDeckSpec extends CatsEffectSuite with ScalaCheckEffectSuite:

  test("it should always contains 52 unique cards") {
    forAllF(Gen.const(())) { _ =>
      for
        sd <- shuffledDeck
        _ <- IO(assert(sd.cards.toSet.size == 52))
      yield ()
    }
  }

  test("it should always contains 52 unique cards") {
    forAll(Gen.const(())) { _ =>
      assert { shuffledDeckOption match {
        case Some(v) => v.cards.toSet.size == 51
        case None => false }
      }
    }
  }

  test("test decks Gen") {
    forAll(shuffledDecks) { deck =>
      assert(deck.cards.toSet.size == 52)
    }
  }

