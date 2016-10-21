

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
def createTsRowIdx[A]
  (record:A)
  (implicit ev: A ∈ DateOrDateTime):TimeSeriesRowIndex = {
  record match {
    case t: LocaDateTime => dateTimeObjAttrs(t)
    case d: LocalDate => dateObjAttrs(d)
  }
}

case class TimeSeriesRowIndex(
  year:Int,
  month:Int,
  dayOfMonth:Int,
  dayOfWeek:Int,
  hour:Int,
  minute:Int,
  second:Int,
  nanoSecond:Int
)


/**
 *
 */
def dateTimeObjAttrs(record:LocalDateTime) = {
  TimeSeriesRowIndex(
    year=record.getYear,
    month=record.getMonthValue,
    dayOfMonth=record.getDayOfMonth,
    dayOfWeek=LutWeekDay(record.getDayOfWeek.toString),
    hour=record.getHour,
    minute=record.getMinute,
    second=record.getSecond,
    nanoSecond=record.getNano
  )
}


/**
 *
 */
def dateObjAttrs(record:LocalDate) = {
  TimeSeriesRowIndex(
    year=record.getYear,
    month=record.getMonthValue,
    dayOfMonth=record.getDayOfMonth,
    dayOfWeek=LutWeekDay(record.getDayOfWeek.toString),
    hour=0,
    minute=0,
    second=0,
    nanoSecond=0
  )
}


def createTsIdx
