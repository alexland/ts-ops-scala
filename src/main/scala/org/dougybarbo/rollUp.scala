

// load.ivy("org.scalanlp" %% "breeze-natives" % "0.12")
// load.ivy("org.scalanlp" %% "breeze" % "0.12")


import java.time._

import java.time.{LocalDateTime,ZoneId,ZonedDateTime}

import java.time.DayOfWeek
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.Month
import java.time.Year
import java.time.temporal.TemporalAdjusters
import java.time.format.DateTimeFormatter

// import breeze.linalg._






/**
  * timeseries index values must be of class:
  * LocalDateTime or
  * ZonedDateTime
  *
  */

val t0 = LocalDateTime.now
// 2016-05-20T13:45:22.350


val currentTime = Instant.now
val myTimeZone = ZoneId.systemDefault
val t0 = currentTime.atZone(myZone)

val f = (t:Instant) => t.atZone(myZone)

t0.getMinute
// 6

t0.getHour
// 13

t0.getDayOfWeek
// FRIDAY

// eg, use t0.getDayOfYear % 7 for rollup by week
t0.getDayOfYear
// 141


t0.getMonth
// MAY

t0.getYear
// 2016



// look-up tables to convert months, weekdays, etc. to integers
val LutMonth = "JANUARY FEBRUARY MARCH APRIL MAY JUNE JULY AUGUST SEPTEMBER OCTOBER NOVEMBER DECEMBER"
                .split(" ")
                .zipWithIndex
                .toMap

val LutWeekDay = "MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY"
                  .split(" ")
                  .zipWithIndex
                  .toMap


val LutMonth_rev = for ((k,v) <- LutMonth) yield {
    (v,k)
  }


  val LutWeekDay_rev = for ((k,v) <- LutWeekDay) yield {
      (v,k)
    }


// TODO: tighten the return value of this fn via a custom union type, etc.


def createTsRowIdx[A](record:A)(implicit ev: A ∈ DateOrDateTime):Array[Int] = {
  val buf = new ArrayBuffer[Int]()
  buf += record.getYear
  buf += record.getMonthValue
  buf += record.getDayOfMonth
  buf += LutWeekDay(record.getDayOfWeek)
  buf += record.getHour
  buf += record.getMinute
  buf += record.getSecond
  buf += record.getNano
}









val numPeriods = 7 * 24 * 60
import collection.mutable.ListBuffer

// create datetime field
var idx = ListBuffer[Instant]()
var currentTime = Instant.now()
var t0 = currentTime
val incr = 3600
var c = 0

for (i <- 1 to numPeriods) {
  c += 1
  idx += t0.minusSeconds(incr * c)
}

val idxTs = idx.toArray.reverse.map(f(_))

val idxV = DenseVector(idxTs)

val idxV1 = idxV(0 to 50)

val idx = idxV1.toArray.toList

val vx = Seq.fill(idxV1.length)(RND.nextDouble).toList

val q = idx.zip(vx)



// -------------------- roll-up operations ----------------- //

val f1 = (t:Tuple2[ZonedDateTime,Double]) => t._1
val f2 = (t:List[Tuple2[ZonedDateTime,Double]]) => t.map(u => u._2)


val q1 = q.groupBy(f1(_).getDayOfYear).mapValues(f2(_).sum)

// q1: Map[Int, Double] = Map(
//                          140 -> 10.35399229462347,
//                          139 -> 3.8705253207950907,
//                          141 -> 10.37014454816531
// )

def thresholdVal(f:List[Double] => Double)(q:List[Double]) = {
  val x = f(q)
  x * 2
}



/**
  * @param tsData time series data as Vector of Tuples2
  * @param aggFn func to map the values over in each group
  * @param toPeriod eg "minutes", "hours", "days", "weeks", "months"
  *
  *
  */
def rollUp(tsData:Vector[Tuple2[ZonedDateTime, Double]])
          (aggFn:Double => Double)
          (toPeriod:String):Vector[Tuple2[ZonedDateTime, Double]]  = {
  val f1 = (t:Tuple2[ZonedDateTime,Double]) => t._1
  val f2 = (t:List[Tuple2[ZonedDateTime,Double]]) => t.map(u => u._2)
  // TODO: case clauses to map 'toPeriod' arg to java.time func
  // such as .getDayOfYear
  val q1 = q.groupBy(f1(_).extractRollUpPeriod).mapValues(f2(_).aggFn)
}




// -------------------- extract range ----------------- //


def f[A](q:List[A]):Map[A,Int] = {
  q.foldLeft(Map[A,Int]()) {
    case (u,v) => u.updated(v, u.getOrElse(v, 0) + 1)
  }
}






idx.zip(d).collect {
  case u if (u._1 == true) => u._2
}




// spire to make the function generic:
    import spire.implicits._
    import spire.math._
    import spire.algebra._

    def filterRows[A:Numeric]
      (M:breeze.linalg.DenseMatrix[A], colId:Int, f:A => Boolean) = {
        val col = M(::,colId)
        val idx = col.findAll(f)
        M(idx,::).toDenseMatrix
    }





    val m = DenseMatrix((5, 7, 8), (7, 1, 3), (6, 7, 4))

    def f[A:Numeric](x:A):Boolean = (x < 6)

    filterRows(m, 1, f)
