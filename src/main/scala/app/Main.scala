package app

import Packet.CardValue
import Packet.CardValue.cardValues
import Packet.ShuffledDeck.{Card, ShuffledDeck, shuffledDeck}
import Packet.Suit.suits
import Packet.Performance.*
import cats.Monad
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*

import scala.util.Random

object Main extends IOApp:
  trait Console[F[_]]:
    def putStrLn(str: String): F[Unit]

    def readLn: F[String]


  val res: IO[(Int, List[String])] =
    for
      deck <- shuffledDeck
      workingHands = dealTheWorkingHands(deck)
      remainingPair = theRemainingPair(workingHands, CardValue.cardValues)
      _ = println(remainingPair)
      result = resultOfTrick(Nil, remainingPair, workingHands.remainderOfDeck)
      formatted = formatDeck(result.deck)
    yield (result.successes, formatted)


  override def run(args: List[String]): IO[ExitCode] =
    res.flatMap(r => IO(println(s"result: $r"))).as(ExitCode.Success)


