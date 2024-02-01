import Packet.{CardValue, Suit}
import Packet.ShuffledDeck.Card
import Packet.Suit.*
import Packet.CardValue.*

List(1, 2, 3, 4, 2, 1).containsSlice(List(1, 2))
val indexOfPair = List(1, 2, 3, 4, 2, 1).indexOfSlice(List(3, 4))
List(1, 2, 3, 4, 2, 1).slice(indexOfPair, indexOfPair + 2)
List(1, 2, 3, 4, 2, 1).drop(indexOfPair + 2)
List(1, 2, 3, 4, 2, 1).sliding(2, 1).toList
List('c', 'a', 'b', 'd', 'r' ,'a', 'b', 'g').sliding(2, 1).toList.foldLeft(List.empty[Char])((acc, lc) => {
  if(lc == List('a', 'b') || lc == List('b', 'a')) acc :+ lc.map(_.toUpper).head else acc ++ lc
})



List(
  Card(Two, Hearts),
  Card(Three, Hearts),
  Card(Four, Clubs),
  Card(Three, Clubs),
  Card(Two, Clubs)
)

List(1, 2, 3) ::: List(4, 5, 6, 6).distinct