

import spire.math._
import spire.implicits._


TODO: write repr methods for TimeSeries case class


// create a union type
type DateOrDateTime = t[java.time.LocalDate]#t[java.time.LocalDateTime]




case class TimeSeries(
            rowId:Int,
            idx:DateOrDateTime,
            v:Double,
            tZone:ZoneId = ZoneId.systemDefault) {

  override def toString = {
    def f[A](a:A)(implicit ev: A ∈ DateOrDateTime):String = {
      a match {
        case d:LocalDate => d.format(DateTimeFormatter
                              .ofPattern("dd-MMM-yy"))
        case dt:LocalDateTime => dt.format(DateTimeFormatter
                                  .ofPattern("dd-MMM-yy:H:m:s"))
      }
    }
    f(idx)
  }
}










override def toString = {
  val idx_ldt = idx.toLocalDateTime
  val dateTimeFormat = "dd-MMM-yy"
  idx_ldt.format(DateTimeFormatter.ofPattern(dateTimeFormat))
}




// 1-hour time step
val incr = 3600
val myTimeZone = ZoneId.systemDefault
// val t0 = Instant.now.atZone(myTimeZone)
val t0 = Instant.now


def createTimeSeries(incr:Int, numPoints:Int):Seq[TimeSeries] = {
  val t0 = Instant.now
  val myTimeZone = ZoneId.systemDefault
  val tx = for (c <- 1 to numPoints) yield {
    TimeSeries(
      t0.plusSeconds(c * incr).atZone(myTimeZone),
      RND.nextDouble
    )
  }
  tx
}






def f(q:Seq[Int]):List[Int] = {
  q.sliding(7, 7).foldLeft(List[Int]()) { (u, v) => u :+ v.sum }
}




def discrFn(ts:Seq[TimeSeries], toPeriod:String):Seq[TimeSeries] = {
  toPeriod match {
  case "minute" => ts.groupBy(_.idx.getMinute)
  case "hour" => ts.groupBy(_.idx.getHour)
  case "day" => ts.groupBy(_.idx.getDayOfYear)
  case "week" => ts.groupBy(_.idx)
  case "month"  => ts.groupBy(_.idx.getMonth)
  case "year" => ts.groupBy(_.idx.getYear)
 }
}


def rollup(ts:Seq[TimeSeries])
    (reducer:Double => Double)
    (toPeriod:String):Seq[TimeSeries] = {
  val ts1 = discrFn(ts, toPeriod)
  val keys = ts1.keys
  for (key <- keys) yield {
    key -> ts1(key).map(_.v).reducer
}








def groupByOp(u: Seq[Int], f: =>)



import java.time.{LocalDateTime,ZoneId,ZonedDateTime}

val numPeriods = 7 * 24 * 60
import collection.mutable.ListBuffer
var idx = ListBuffer[Instant]()
var currentTime = Instant.now()
var t0 = currentTime
val incr = 3600
var c = 0
