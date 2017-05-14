package org.quantlib.time

import org.quantlib.time.enums.TimeUnit
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
import org.quantlib.time.standards._
import org.scalatest._

/**
  * Created by neo on 12/03/2017.
  */
class StandardFormatSpec extends FlatSpec with Matchers {

  "ecbDates" should "pass all test" in {
    import org.quantlib.time.implicits.Date._

   // val ecb = ECB()
   // val knownDates = ecb.knownDates
    assert(true)
  }

  "immDates" should "pass all test" in {

    import org.quantlib.time.implicits.Date._
    val codes = List(
      "F0", "G0", "H0", "J0", "K0", "M0", "N0", "Q0", "U0", "V0", "X0", "Z0",
      "F1", "G1", "H1", "J1", "K1", "M1", "N1", "Q1", "U1", "V1", "X1", "Z1",
      "F2", "G2", "H2", "J2", "K2", "M2", "N2", "Q2", "U2", "V2", "X2", "Z2",
      "F3", "G3", "H3", "J3", "K3", "M3", "N3", "Q3", "U3", "V3", "X3", "Z3",
      "F4", "G4", "H4", "J4", "K4", "M4", "N4", "Q4", "U4", "V4", "X4", "Z4",
      "F5", "G5", "H5", "J5", "K5", "M5", "N5", "Q5", "U5", "V5", "X5", "Z5",
      "F6", "G6", "H6", "J6", "K6", "M6", "N6", "Q6", "U6", "V6", "X6", "Z6",
      "F7", "G7", "H7", "J7", "K7", "M7", "N7", "Q7", "U7", "V7", "X7", "Z7",
      "F8", "G8", "H8", "J8", "K8", "M8", "N8", "Q8", "U8", "V8", "X8", "Z8",
      "F9", "G9", "H9", "J9", "K9", "M9", "N9", "Q9", "U9", "V9", "X9", "Z9"
    )

    var counter = DateOps.MIN

    val last = DateOps.MIN + Period(121, TimeUnit.Months)

    while (counter <= last) {

      val immDateOption = IMM.nextDate(counter, false)

      immDateOption foreach { immDate =>

        assert(immDate > counter,
          s"\n  %${immDate.getDayOfMonth} imm is not greater than ${counter.getDayOfMonth} counter")

        assert(IMM.confirm(immDate, false),
          s"\n ${immDate.getDayOfMonth} imm is not an IMM date (calculated from ${counter.getDayOfMonth})")

        IMM.nextDate(counter, true) foreach { immDateInMainCycle =>
          assert(immDate <= immDateInMainCycle,
            s"\n ${immDate.getDayOfMonth} imm is notless than or equal to " +
              s"the next future in the main cycle: ${immDateInMainCycle.getDayOfMonth})")
        }
        assert(IMM.toDate(IMM.toCode(immDate),counter).contains(immDate)
          ,s"\n ${IMM.toCode(immDate)} at calendar day $counter is not the IMM code matching $immDate")

      }

      codes foreach { code =>
        assert(IMM.toDate(code, counter).exists(_ >= counter))
      }

      counter = counter + 1
    }

  }

  "asxDates" should "pass all test" in {
    import org.quantlib.time.implicits.Date._

    val codes = List(
      "F0", "G0", "H0", "J0", "K0", "M0", "N0", "Q0", "U0", "V0", "X0", "Z0",
      "F1", "G1", "H1", "J1", "K1", "M1", "N1", "Q1", "U1", "V1", "X1", "Z1",
      "F2", "G2", "H2", "J2", "K2", "M2", "N2", "Q2", "U2", "V2", "X2", "Z2",
      "F3", "G3", "H3", "J3", "K3", "M3", "N3", "Q3", "U3", "V3", "X3", "Z3",
      "F4", "G4", "H4", "J4", "K4", "M4", "N4", "Q4", "U4", "V4", "X4", "Z4",
      "F5", "G5", "H5", "J5", "K5", "M5", "N5", "Q5", "U5", "V5", "X5", "Z5",
      "F6", "G6", "H6", "J6", "K6", "M6", "N6", "Q6", "U6", "V6", "X6", "Z6",
      "F7", "G7", "H7", "J7", "K7", "M7", "N7", "Q7", "U7", "V7", "X7", "Z7",
      "F8", "G8", "H8", "J8", "K8", "M8", "N8", "Q8", "U8", "V8", "X8", "Z8",
      "F9", "G9", "H9", "J9", "K9", "M9", "N9", "Q9", "U9", "V9", "X9", "Z9"
    )
    var counter = DateOps.MIN

    val last = DateOps.MIN + Period(121, TimeUnit.Months)

    while (counter <= last) {

      val asxDateOption = ASX.nextDate(counter, false)

      asxDateOption foreach { asxDate =>

        assert(asxDate > counter,
          s"\n  %${asxDate.getDayOfMonth} asx is not greater than ${counter.getDayOfMonth} counter")

        assert(ASX.confirm(asxDate, false),
          s"\n ${asxDate.getDayOfMonth} asx is not an ASX date (calculated from ${counter.getDayOfMonth})")

        ASX.nextDate(counter, true) foreach { asxDateInMainCycle =>
          assert(asxDate <= asxDateInMainCycle,
            s"\n ${asxDate.getDayOfMonth} asx is notless than or equal to " +
              s"the next future in the main cycle: ${asxDateInMainCycle.getDayOfMonth})")
        }
        assert(ASX.toDate(ASX.toCode(asxDate),counter).contains(asxDate)
          ,s"\n ${ASX.toCode(asxDate)} at calendar day $counter is not the ASX code matching $asxDate")

      }

      codes foreach { code =>
        assert(ASX.toDate(code, counter).exists(_ >= counter))
      }

      counter = counter + 1
    }

  }

}
