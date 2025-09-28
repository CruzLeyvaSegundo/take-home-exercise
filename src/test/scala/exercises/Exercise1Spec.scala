package exercises

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class Exercise1Spec extends AnyFlatSpec with Matchers {
  import exercises.Exercise1._

  val rates = Seq(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )

  val ratesWithNotMatchData = Seq(
    Rate("R1", "Test1"),
    Rate("R1", "Test1"),
    Rate("P1", "Test2"),
    Rate("P2", "Test2")
  )

  val cabinPrices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )

  "getBestGroupPrices" should "return the best groupPrices per rate and cabin" in {
    val currentResult = getBestGroupPrices(rates, cabinPrices)

    val expectedResult = Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior")
    )

    currentResult should contain theSameElementsAs expectedResult
  }

  "getBestGroupPrices" should "return empty Seq when there is no matches between rates and cabin prices" in {
    val currentResult = getBestGroupPrices(ratesWithNotMatchData, cabinPrices)

    currentResult.size should be(0)
  }

  "getBestGroupPrices" should "return empty Seq when there is no rates" in {
    val currentResult = getBestGroupPrices(Seq(), cabinPrices)

    currentResult.size should be(0)
  }

  "getBestGroupPrices" should "return empty Seq when there is no cabinPrices" in {
    val currentResult = getBestGroupPrices(rates, Seq())

    currentResult.size should be(0)
  }
}
