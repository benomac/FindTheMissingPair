package Support

import Packet.ShuffledDeck.shuffledDeck
import munit.ScalaCheckSuite

class ShuffledDeckSpec extends ScalaCheckSuite:

  test("it should always contain 52 cards") {
    1.to(10000).foreach(_ => assert(shuffledDeck.cards.toSet.size == 52))
  }



