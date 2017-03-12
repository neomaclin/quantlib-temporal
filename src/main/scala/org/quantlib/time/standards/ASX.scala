package org.quantlib.time.standards

import org.quantlib.time.implicits.DateOps
/**
  * Created by neo on 09/03/2017.
  */
object ASX{

  //! returns whether or not the given date is an ASX date
  def isASXdate[D: DateOps](date: D, mainCycle: Boolean): Boolean = ???

  //! returns whether or not the given string is an ASX code
  def isASXcode(in: String, mainCycle: Boolean): Boolean = ???

  /*! returns the ASX code for the given date
      (e.g. M5 for June 12th, 2015).

      \warning It raises an exception if the input
               date is not an ASX date
  */
  def code[D: DateOps](asxDate: D): String = ???

  /*! returns the ASX date for the given ASX code
      (e.g. June 12th, 2015 for M5).

      \warning It raises an exception if the input
               string is not an ASX code
  */
  def date[D: DateOps](asxCode: String, referenceDate: Option[D]): D = ???

  //! next ASX date following the given date
  /*! returns the 1st delivery date for next contract listed in the
      Australian Securities Exchange.
  */
  def nextDate[D: DateOps](date: D, mainCycle: Boolean): D = ???

  //! next ASX date following the given ASX code
  /*! returns the 1st delivery date for next contract listed in the
      Australian Securities Exchange
  */
  def nextDate[D: DateOps](asxCode: String, mainCycle: Boolean, referenceDate: Option[D]): D = ???

  //! next ASX code following the given date
  /*! returns the ASX code for next contract listed in the
      Australian Securities Exchange
  */
  def nextCode[D: DateOps](date: D, mainCycle: Boolean): String = ???

  //! next ASX code following the given code
  /*! returns the ASX code for next contract listed in the
      Australian Securities Exchange
  */
  def nextCode[D: DateOps](asxCode: String, mainCycle: Boolean, referenceDate: Option[D]): String = ???


}
