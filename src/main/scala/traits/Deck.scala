package traits

import Packet.ShuffledDeck.ShuffledDeck

trait Deck[F[_]]:
  def shuffledDeck: F[ShuffledDeck]