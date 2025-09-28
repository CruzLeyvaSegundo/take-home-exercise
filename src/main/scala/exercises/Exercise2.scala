package exercises

import scala.annotation.tailrec

object Exercise2 {
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  case class PromotionComboCandidate(promotionCodes: Seq[String], notCombinableWith: Seq[String]) {
    def toPromotionCombo: PromotionCombo = PromotionCombo(promotionCodes)
  }

  def generatePromotionComboCandidates(
    allPromotions: Seq[Promotion],
    currentPivotCandidate: PromotionComboCandidate
  ): Seq[PromotionComboCandidate] = {
    allPromotions
      .filter(promotion =>
        !currentPivotCandidate.promotionCodes.contains(promotion.code)
          && !currentPivotCandidate.promotionCodes.exists( promotionCode =>
          promotion.notCombinableWith.contains(promotionCode)
        )
          && !currentPivotCandidate.notCombinableWith.contains(promotion.code)
      )
      .map( promotion =>
        PromotionComboCandidate(
          currentPivotCandidate.promotionCodes :+ promotion.code,
          (currentPivotCandidate.notCombinableWith ++ promotion.notCombinableWith).distinct
        )
      )
  }

  def optimizePromotionComboCandidates(
    promotionComboCandidates: Seq[PromotionComboCandidate],
    visitedCombinations: Seq[PromotionComboCandidate]
  ): Seq[PromotionComboCandidate] = promotionComboCandidates
    .filter(promotionComboCandidate =>
      !visitedCombinations.exists( visitedCombination =>
        visitedCombination.promotionCodes.toSet == promotionComboCandidate.promotionCodes.toSet
      )
    )

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

    @tailrec
    def combinePromotionsWithRestrictions(
      allPromotions: Seq[Promotion],
      candidates: Seq[PromotionComboCandidate],
      visitedCombinations: Seq[PromotionComboCandidate] = Seq(),
      solutions: Seq[PromotionComboCandidate] = Seq()
    ): Seq[PromotionComboCandidate] = candidates match {
      case candidate::rest =>
        val promotionComboCandidates = generatePromotionComboCandidates(allPromotions, candidate)

        // return current candidate as a solution if there is no more new promotion combo candidates
        val solution = if(promotionComboCandidates.isEmpty) Some(candidate) else None

        //optimize candidates combo search solutions by not repeating steps for already visited candidates
        val optimizedPromotionComboCandidates = optimizePromotionComboCandidates(
          promotionComboCandidates,
          visitedCombinations
        )

        combinePromotionsWithRestrictions(
          allPromotions,
          optimizedPromotionComboCandidates ++ rest,
          visitedCombinations ++ optimizedPromotionComboCandidates ++ solution,
          solutions ++ solution
        )
      case Nil => solutions
    }

    val pivotCandidates = allPromotions.map(promotion =>
      PromotionComboCandidate(
        Seq(promotion.code),
        promotion.notCombinableWith
      )
    )

    combinePromotionsWithRestrictions(
      allPromotions,
      pivotCandidates
    ).map(_.toPromotionCombo)
  }

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = allCombinablePromotions(allPromotions)
    .filter(_.promotionCodes.contains(promotionCode))
    .sortBy(_.promotionCodes.size)
}