package exercises

import scala.annotation.tailrec

object Exercise2 {
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  case class PromotionComboCandidate(promotionCodes: Seq[String], notCombinableWith: Seq[String]) {
    def toPromotionCombo: PromotionCombo = PromotionCombo(promotionCodes)
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

    @tailrec
    def combinePromotionsWithRestrictions(
      candidates: Seq[PromotionComboCandidate],
      visitedCombinations: Seq[PromotionComboCandidate] = Seq(),
      solutions: Seq[PromotionComboCandidate] = Seq()
    ): Seq[PromotionComboCandidate] = {
      candidates match {
        case candidate::rest =>
          val rawPromotionComboCandidates = allPromotions
            .filter(promotion =>
              !candidate.promotionCodes.contains(promotion.code)
                && !candidate.promotionCodes.exists( promotionCode =>
                promotion.notCombinableWith.contains(promotionCode)
              )
                && !candidate.notCombinableWith.contains(promotion.code)
            )
            .map( promotion =>
              PromotionComboCandidate(
                candidate.promotionCodes :+ promotion.code,
                (candidate.notCombinableWith ++ promotion.notCombinableWith).distinct
              )
            )

          val solution = if(rawPromotionComboCandidates.isEmpty) Some(candidate) else None

          //optimize candidates combo search solutions by not repeating steps for already visited candidates
          val optimizedPromotionComboCandidates = rawPromotionComboCandidates
            .filter(promotionComboCandidate =>
              !visitedCombinations.exists( visitedCombination =>
                visitedCombination.promotionCodes.toSet == promotionComboCandidate.promotionCodes.toSet
              )
            )

          combinePromotionsWithRestrictions(
            optimizedPromotionComboCandidates ++ rest,
            visitedCombinations ++ optimizedPromotionComboCandidates ++ solution,
            solutions ++ solution
          )
        case Nil =>
          solutions
      }
    }

    val pivotCandidates = allPromotions.map(promotion =>
      PromotionComboCandidate(
        Seq(promotion.code),
        promotion.notCombinableWith
      )
    )

    combinePromotionsWithRestrictions(
      pivotCandidates
    ).map(_.toPromotionCombo)
  }

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    allCombinablePromotions(allPromotions)
      .filter(_.promotionCodes.contains(promotionCode))
      .sortBy(_.promotionCodes.size)
  }
}