package Support

import Packet.{CardValue, Suit}
import Packet.ShuffledDeck.{Card, ShuffledDeck}
import org.scalacheck.Gen
import org.scalacheck.Gen.*

import scala.util.Random

object DeckGens:
    def suits: Gen[List[Suit]] = Gen.const(Suit.suits)
    def values: Gen[List[CardValue]] = Gen.const(CardValue.cardValues)
    def shuffledDecks: Gen[ShuffledDeck] = {
      for {
        v <- values
        s <- suits
        deck = v.flatMap(value => s.map(suit => Card(value, suit)))
      } yield ShuffledDeck(Random.shuffle(deck))
    }







