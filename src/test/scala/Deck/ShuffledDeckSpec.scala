package Deck

import Packet.ShuffledDeck.shuffledDeck
import Packet.ShuffledDeck.shuffledDeck
import Support.DeckGens.shuffledDecks
import cats.effect.{IO, IOApp, Sync}
import munit.ScalaCheckSuite
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.effect.PropF.forAllF
import traits.Deck

class ShuffledDeckSpec extends CatsEffectSuite with ScalaCheckEffectSuite:

  test("it should always contains 52 unique cards") {
    forAllF(Gen.const(())) { _ =>
      for
        sd <- shuffledDeck
        _ <- IO(assert(sd.cards.toSet.size == 52))
      yield ()
    }
  }

  test("test decks Gen") {
    forAll(shuffledDecks) { deck =>
      assert(deck.cards.toSet.size == 52)
    }
  }

