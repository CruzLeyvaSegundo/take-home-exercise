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

  "generatePromotionComboCandidates" should "return next promotion combo candidates based on the current pivot candidate" in {
    val currentPivot = PromotionComboCandidate(Seq("P1"), Seq("P3"))
    val result = generatePromotionComboCandidates(
      promotions,
      currentPivot
    )

    val expectedResult = Seq(
      PromotionComboCandidate(Seq("P1", "P2"), Seq("P3", "P4", "P5")),
      PromotionComboCandidate(Seq("P1", "P4"), Seq("P3", "P2")),
      PromotionComboCandidate(Seq("P1", "P5"), Seq("P3", "P2"))
    )

    result should contain theSameElementsAs expectedResult
  }

  "generatePromotionComboCandidates" should "return empty Seq when there is no more possible candidates" in {
    val currentPivot = PromotionComboCandidate(Seq("P1", "P4", "P5"), Seq("P3", "P2"))
    val result = generatePromotionComboCandidates(
      promotions,
      currentPivot
    )

    result.isEmpty should be(true)
  }

  "optimizePromotionComboCandidates" should "return optimized next promotion combo candidates based in memory visited candidates" in {
    val promotionComboCandidates = Seq(
      PromotionComboCandidate(Seq("P2", "P1"), Seq("P4", "P5", "P3")),
      PromotionComboCandidate(Seq("P2", "P3"), Seq("P4", "P5", "P1"))
    )
    val visitedCombinations = Seq(
      PromotionComboCandidate(Seq("P1", "P2"), Seq("P3", "P4", "P5")),
      PromotionComboCandidate(Seq("P1", "P4"), Seq("P3", "P2")),
      PromotionComboCandidate(Seq("P1", "P5"), Seq("P3", "P2"))
    )

    val result = optimizePromotionComboCandidates(
      promotionComboCandidates,
      visitedCombinations
    )

    val expectedResult = Seq(
      PromotionComboCandidate(Seq("P2", "P3"), Seq("P4", "P5", "P1"))
    )

    result should contain theSameElementsAs expectedResult
  }

  "optimizePromotionComboCandidates" should "return empty Seq when there is no more possible candidates after optimization using in memory visited candidates" in {
    val promotionComboCandidates = Seq(
      PromotionComboCandidate(Seq("P5", "P1"), Seq("P2", "P3")),
      PromotionComboCandidate(Seq("P5", "P3"), Seq("P2", "P1")),
      PromotionComboCandidate(Seq("P5", "P4"), Seq("P2")),
    )

    val visitedCombinations = Seq(
      PromotionComboCandidate(Seq("P1", "P5"), Seq("P3", "P2")),
      PromotionComboCandidate(Seq("P3", "P5"), Seq("P1", "P2")),
      PromotionComboCandidate(Seq("P4", "P5"), Seq("P2")),
    )

    val result = optimizePromotionComboCandidates(
      promotionComboCandidates,
      visitedCombinations
    )

    result.isEmpty should be(true)
  }

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

  "allCombinablePromotions" should "return empty Seq when there is no promotions" in {
    val result = allCombinablePromotions(Seq())

    result.isEmpty should be(true)
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

  "combinablePromotions" should "return empty Seq when the given promotion code doesn't exists in the all promotions" in {
    val result = combinablePromotions("E1", promotions)

    result.isEmpty should be(true)
  }
}
