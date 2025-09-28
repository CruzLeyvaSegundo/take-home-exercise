package exercises

object Exercise1 {
  case class Rate(rateCode: String, rateGroup: String)
  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
  case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val rateCodesByRateGroup = rates.groupMap(_.rateGroup)(_.rateCode).toSeq

    val bestGroupPrices = rateCodesByRateGroup
      .flatMap { case (rateGroup, rateCodes) =>
        val cabinPricesCandidates = prices
          .filter(cabinPrice => rateCodes.contains(cabinPrice.rateCode))

        val bestGroupPricesByRateGroupAndCabinCode = cabinPricesCandidates
          .groupBy(_.cabinCode)
          .map { case (_, cabinPrices) =>
            val bestCabinPriceCandidateByCabinCode = cabinPrices.minBy(_.price)

            BestGroupPrice(
              bestCabinPriceCandidateByCabinCode.cabinCode,
              bestCabinPriceCandidateByCabinCode.rateCode,
              bestCabinPriceCandidateByCabinCode.price,
              rateGroup
            )
          }
          .toSeq

        bestGroupPricesByRateGroupAndCabinCode
      }

    bestGroupPrices
      .sortBy(groupPrice =>(groupPrice.cabinCode, groupPrice.rateCode))
  }

}