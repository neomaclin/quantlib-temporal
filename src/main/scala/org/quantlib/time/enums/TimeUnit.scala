package org.quantlib.time.enums

/**
  * Created by neo on 11/03/2017.
  */
sealed trait TimeUnit

object TimeUnit {

  case object Days extends TimeUnit

  case object Weeks extends TimeUnit

  case object Months extends TimeUnit

  case object Years extends TimeUnit

  case object Hours extends TimeUnit

  case object Minutes extends TimeUnit

  case object Seconds extends TimeUnit

  case object Milliseconds extends TimeUnit

  case object Microseconds extends TimeUnit

}
