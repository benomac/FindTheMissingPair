package Deck

import Packet.ShuffledDeck.shuffleDeck
import Support.DeckGens.shuffledDecks
import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.effect.PropF.forAllF

class ShuffledDeckSpec extends CatsEffectSuite with ScalaCheckEffectSuite:

  test("it should always contains 52 unique cards") {
    forAllF(Gen.const(())) { _ =>
      assertIO(shuffleDeck.map(_.cards.toSet.size), 52)
    }
  }

  test("test decks Gen") {
    forAll(shuffledDecks) { deck =>
      assert(deck.cards.toSet.size == 52)
    }
  }

