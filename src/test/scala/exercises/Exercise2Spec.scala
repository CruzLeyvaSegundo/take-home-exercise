package exercises

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec


class Exercise2Spec extends AnyFlatSpec with Matchers {
  import exercises.Exercise2._

  val promotions = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2")) // P5 is not combinable with P2
  )

  "allCombinablePromotions" should "return all the combinable promotions considering the not combinable restrictions" in {
    val result = allCombinablePromotions(promotions)

    val expectedResult = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    result should contain theSameElementsAs expectedResult
  }

  "allCombinablePromotions" should "return empty List when there is no promotions" in {
    val result = allCombinablePromotions(Seq())

    result.size should be(0)
  }

  "combinablePromotions" should "return all the promotion combos for a given promotion code" in {
    val result1 = combinablePromotions("P1", promotions)
    val result2 = combinablePromotions("P3", promotions)

    // Expected Output for Promotion Combinations for promotionCode=”P1”:
    val expectedResult1 = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5"))
    )

    //Expected Output for Promotion Combinations for promotionCode=”P3”:
    val expectedResult2 = Seq(
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    result1 should contain theSameElementsAs expectedResult1
    result2 should contain theSameElementsAs expectedResult2
  }

  "combinablePromotions" should "return empty List when the given promotion code doesn't exists in the given promotions" in {
    val result = combinablePromotions("E1", promotions)

    result.size should be(0)
  }
}
