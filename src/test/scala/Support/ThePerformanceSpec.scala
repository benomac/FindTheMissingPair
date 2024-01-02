//package Support
//
//import Packet.ThePerformance.*
//import Packet.ThePerformance.dealTheHands
//import Support.DeckGens.shuffledDecks
//import munit.ScalaCheckSuite
//import org.scalacheck.Prop.forAll
//
//class ThePerformanceSpec extends ScalaCheckSuite:

//  test("all working hands should have eleven unique cards") {
//    forAll(shuffledDecks) { deck =>
//      for
//        d <- deck
//      yield ???
////      val workingHands: WorkingHands = dealTheHands(deck)
////      assert(workingHands.elevenUniqueCards.size == 52)
//    }
//  }