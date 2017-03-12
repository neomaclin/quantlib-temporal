package org.quantlib.time.enums

/**
  * Created by neo on 11/03/2017.
  */
sealed trait DateGenerationRule

object DateGenerationRule {

  case object Backward extends DateGenerationRule

  case object Forward extends DateGenerationRule

  case object Zero extends DateGenerationRule

  case object ThirdWednesday extends DateGenerationRule

  case object Twentieth extends DateGenerationRule

  case object TwentiethIMM extends DateGenerationRule

  case object OldCDS extends DateGenerationRule

  case object CDS extends DateGenerationRule

}