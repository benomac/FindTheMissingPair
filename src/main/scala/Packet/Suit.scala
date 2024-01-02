package Packet


enum Suit:
  case Spades extends Suit
  case Clubs extends Suit
  case Hearts extends Suit
  case Diamonds extends Suit
  
object Suit:
  def suits: List[Suit] = Suit.values.toList
  
