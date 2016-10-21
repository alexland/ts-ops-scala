

import java.time.LocalDate
import java.time.LocalDateTime

import scala.collection.mutable.ArrayBuffer

import scalaz.UnionTypes._


/**
  * timeseries index values must be of class:
  * LocalDateTime or
  * ZonedDateTime
  *
  */


// look-up tables to convert months, weekdays, etc. to integers
val LutMonth = "JANUARY FEBRUARY MARCH APRIL MAY JUNE JULY AUGUST SEPTEMBER OCTOBER NOVEMBER DECEMBER"
                .split(" ")
                .zipWithIndex
                .toMap

val LutWeekDay = "MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY"
                  .split(" ")
                  .zipWithIndex
                  .toMap

val LutMonth_rev = for ((k,v) <- LutMonth) yield (v,k)

val LutWeekDay_rev = for ((k,v) <- LutWeekDay) yield (v,k)



// create disjoint type

type DateOrDateTime = t[LocalDate]#t[LocalDateTime]
implicitly[LocalDate ∈ DateOrDateTime]
implicitly[LocalDateTime ∈ DateOrDateTime]

/**
 * @param record has type DateorDateTime, created via
 * LocalDateTime.now or LocalDate.now
 * creates one row of the 2D timeseries index for each timeseries record
 */
def createTsRowIdx[A](record:A)(implicit ev: A ∈ DateOrDateTime):Array[Int] = {
  record match {
    case t: LocaDateTime => dateTimeObjAttrs(t)
    case d: LocalDate => dateObjAttrs(d)
  }
}

def dateTimeObjAttrs(record:LocalDateTime) = {
  val buf = new ArrayBuffer[Int]()
  buf += record.getYear
  buf += record.getMonthValue
  buf += record.getDayOfMonth
  buf += LutWeekDay(record.getDayOfWeek.toString)
  buf += record.getHour
  buf += record.getMinute
  buf += record.getSecond
  buf += record.getNano
  buf.toArray
}

def dateObjAttrs(record:LocalDate) = {
  val buf = new ArrayBuffer[Int]()
  buf += record.getYear
  buf += record.getMonthValue
  buf += record.getDayOfMonth
  buf += LutWeekDay(record.getDayOfWeek.toString)
  buf ++= ArrayBuffer(0, 0, 0, 0)
  buf.toArray
}
