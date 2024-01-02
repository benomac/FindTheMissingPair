package Packet




enum CardValue:

  case Ace extends CardValue

  case Two extends CardValue

  case Three extends CardValue

  case Four extends CardValue

  case Five extends CardValue

  case Six extends CardValue

  case Seven extends CardValue

  case Eight extends CardValue

  case Nine extends CardValue

  case Ten extends CardValue

  case Jack extends CardValue

  case Queen extends CardValue

  case King extends CardValue
  
object CardValue:
  def cardValues: List[CardValue] = CardValue.values.toList
