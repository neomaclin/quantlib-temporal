package org.quantlib.time.implicits

/**
  * Created by neo on 12/03/2017.
  */
object Offset {

  val MonthOffset = List(
    0, 31, 59, 90, 120, 151, // Jan - Jun
    181, 212, 243, 273, 304, 334 // Jun - Dec
  )
  val MonthLeapOffset = List(
    0, 31, 60, 91, 121, 152, // Jan - Jun
    182, 213, 244, 274, 305, 335 // Jun - Dec
  )

}
