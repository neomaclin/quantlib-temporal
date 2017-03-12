package org.quantlib.time


import org.quantlib.time.enums.TimeUnit._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by neo on 12/03/2017.
  */
class PeriodSpec  extends FlatSpec with Matchers {

  "Years Months Algebra of Period" should "pass all testing for default policy." in {
    val oneYear = Period(1, Years)
    val sixMonths = Period(6, Months)
    val threeMonths = Period(3, Months)

    assert(oneYear / 4 == threeMonths, s"division error: $oneYear / 4 not equal to $threeMonths")
    assert(oneYear / 2 == sixMonths, s"division error: $oneYear / 2 not equal to $sixMonths")

    var sum = threeMonths + sixMonths
    assert(sum == Period(9, Months), s"sum error: $threeMonths + $sixMonths != ${Period(9, Months)}")

    sum = sum + oneYear
    assert(sum == Period(21, Months),  s"sum error: $threeMonths + $sixMonths + $oneYear != ${Period(21, Months)}")

    val normalizedTwelveMonths = Period(12, Months).normalize
    assert(normalizedTwelveMonths.length == 1,
      s"normalization error: NormalizedTwelveMonths.length is ${normalizedTwelveMonths.length} instead of 1")
    assert(normalizedTwelveMonths.unit  == Years,
      s"normalization error: NormalizedTwelveMonths.units is ${normalizedTwelveMonths.unit} instead of Years")

  }

  "Weeks Days Algebra of Period" should "pass all testing for default policy." in {
    val TwoWeeks = Period(2, Weeks)
    val OneWeek  = Period(1, Weeks)
    val ThreeDays  = Period(3, Days)
    val OneDay  = Period(1, Days)

    assert(TwoWeeks / 2 == OneWeek, s"division error: $TwoWeeks / 2 not equal to $OneWeek")
    assert(OneWeek / 7 == OneDay, s"division error: $OneWeek / 7 not equal to $OneDay")

    var sum = ThreeDays + OneDay
    assert(sum == Period(4, Days), s"sum error: $ThreeDays + $OneDay != ${Period(4, Days)}")

    sum = sum + OneWeek
    assert(sum == Period(11, Days), s"sum error:  $ThreeDays + $OneDay + $OneWeek != ${Period(11, Days)}")

    val oneWeek = Period(7, Days).normalize
    assert(oneWeek.length == 1,
      s"normalization error: oneWeek.length is ${oneWeek.length} instead of 1")
    assert(oneWeek.unit  == Weeks,
    s"normalization error: oneWeek.units is ${oneWeek.unit} instead of Weeks")

  }
}
