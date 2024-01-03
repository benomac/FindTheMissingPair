package Deck

import Packet.ShuffledDeck.shuffledDeck
import Support.DeckGens.shuffledDecks
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class ShuffledDeckSpec extends ScalaCheckSuite:

  test("it should always contain 52 cards") {
    1.to(10000).foreach(_ => assert(shuffledDeck.cards.toSet.size == 52))
  }

  test("test decks Gen") {
    forAll(shuffledDecks) { deck =>
      assert(deck.cards.toSet.size == 52)
    }
  }

