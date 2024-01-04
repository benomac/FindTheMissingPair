package app

import Packet.CardValue.cardValues
import Packet.ShuffledDeck.{Card, ShuffledDeck}
import Packet.Suit.suits
import Packet.Performance.{dealTheWorkingHands, isThePairTogetherInTheRemainder}
import cats.Monad
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*

import scala.util.Random
object Main extends IOApp {
  trait Console[F[_]]:
    def putStrLn(str: String): F[Unit]
    def readLn: F[String]
    def shuffledDeck: F[ShuffledDeck]

  implicit object ConsoleIO extends Console[IO] {
    override def putStrLn(str: String): IO[Unit] = IO(println(str))
    override def readLn: IO[String] = IO(scala.io.StdIn.readLine)

    override def shuffledDeck: IO[ShuffledDeck] =
      val deck = for {
        card <- cardValues
        suit <- suits
      } yield Card(card, suit)
      IO(ShuffledDeck(Random.shuffle(deck)))
  }
  def program2[F[_] : Monad](implicit C: Console[F]): F[Unit] =
    for {
      shuffledDeck <- C.shuffledDeck
      _ <- C.putStrLn(s"$shuffledDeck")
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program2[IO].as(ExitCode.Success)

}
