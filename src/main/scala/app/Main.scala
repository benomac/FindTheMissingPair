package app

import Packet.CardValue
import Packet.CardValue.cardValues
import Packet.ShuffledDeck.{Card, renderDeck, shuffleDeck}
import Packet.Suit.suits
import Packet.Performance.*
import cats.Monad
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*
import Packet.Suit._
import Packet.CardValue._

import scala.util.Random

object Main extends IOApp:
  trait Console[F[_]]:
    def putStrLn(str: String): F[Unit]

    def readLn: F[String]

  val remainderOfDeck = List(Card(Ten, Spades), Card(Queen, Spades), Card(Jack, Spades), Card(Jack, Clubs), Card(Jack, Diamonds), Card(Ace, Diamonds), Card(Three, Clubs), Card(Ten, Diamonds), Card(Three, Hearts), Card(Five, Hearts), Card(Eight, Spades), Card(Nine, Spades), Card(Six, Diamonds), Card(Eight, Diamonds), Card(Two, Spades), Card(Nine, Diamonds), Card(Six, Hearts), Card(Six, Clubs), Card(Seven, Spades), Card(Five, Spades), Card(Ten, Clubs), Card(Ace, Hearts), Card(Seven, Clubs), Card(King, Spades), Card(Queen, Clubs), Card(Seven, Hearts), Card(Two, Diamonds), Card(Five, Diamonds), Card(Ace, Clubs), Card(Four, Hearts), Card(Three, Diamonds), Card(King, Clubs), Card(Queen, Diamonds), Card(Two, Hearts), Card(King, Diamonds), Card(Nine, Clubs), Card(Two, Clubs), Card(Ten, Hearts), Card(Four, Clubs))
  val pair = PairToLookFor(List(Two, Ten))
  val res: IO[(Int, List[String])] =
    for
      deck <- shuffleDeck
      workingHands = dealTheWorkingHands(deck)
      remainingPair = theRemainingPair(workingHands, CardValue.cardValues)
      _ = println(remainingPair)
      result = resultOfTrick(Nil, remainingPair, workingHands.remainderOfDeck)
      rendered = renderDeck(result.deck)
    yield (result.successes, rendered)


  override def run(args: List[String]): IO[ExitCode] =
    res.flatMap(r => IO(println(s"result: $r"))).as(ExitCode.Success)


