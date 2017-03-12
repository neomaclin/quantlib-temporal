package org.quantlib.time.enums

/**
  * Created by neo on 11/03/2017.
  */

sealed abstract class Frequency(val value: Int) {}

object Frequency {

  case object NoFrequency extends Frequency(-1)

  case object Once extends Frequency(0)

  case object Annual extends Frequency(1)

  case object Semiannual extends Frequency(2)

  case object EveryFourthMonth extends Frequency(3)

  case object Quarterly extends Frequency(4)

  case object Bimonthly extends Frequency(6)

  case object Monthly extends Frequency(12)

  case object EveryFourthWeek extends Frequency(13)

  case object Biweekly extends Frequency(26)

  case object Weekly extends Frequency(52)

  case object Daily extends Frequency(365)

  case object OtherFrequency extends Frequency(999)

}