package org.quantlib.time.calendars


/**
  * Created by neo on 11/03/2017.
  */
//final case class BespokeCalendar[D: DateOps](name: String, weekends: List[D]) extends BusinessCalendar{
//
//  override val toString: String = s"$name : of weekends (${weekends.mkString(",")})"
//
//  override def isWeekend[D:DateOps](date: D): Boolean = weekends.contains(date)
//
//  override def considerBusinessDay[D: DateOps](date: D): Boolean = !weekends.contains(date)
//
//  def addWeekend[D:DateOps](date: D): BespokeCalendar[D] = copy(name, weekends = date :: weekends)
//
//}
