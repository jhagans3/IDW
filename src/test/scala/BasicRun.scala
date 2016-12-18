import myIO._
import myMath._
import myTypes._
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime}
import java.io.{BufferedWriter, FileWriter}


object BasicRun extends App {
//  val name = "blkgrp"
  val name = "county"
  val startTime = LocalDateTime.now()
  println(startTime)

  implicit val fileMap: MapPM25Values = file2Map("input/pm25.txt")
  implicit val radiusForDistance = 2f
  val locations: MapLocationValues = locationFile2Map(s"input/$name.txt")

  val extensionCounty7_5 = locations.par map { case (location, idValues) =>
    val id: Long = idValues.idNumber
    val date: LocalDate = LocalDate.ofYearDay(2009, location.t.toInt)
    val year: Int = date.getYear
    val month: Int = date.getMonthValue
    val day: Int = date.getDayOfMonth
    val interpolation: Double = location idwExtensionC(7, 5f)

    (id, year, month, day, interpolation)
  }

  val endTime = LocalDateTime.now()
  val minDif =  ChronoUnit.MINUTES.between(startTime, endTime)
  val secDiff = ChronoUnit.SECONDS.between(startTime, endTime)

  val header = "(county_id, year, month, day, pm25) \n"
  val w = new BufferedWriter(new FileWriter(s"${name}_id_t_w_test.txt"))

  w.write(header)
  extensionCounty7_5 foreach { interpolation =>
    w.write(s"$interpolation\n")
  }


  //meterics
  w.write("\n")
  w.write(startTime.toString)
  w.write("\n")
  w.write(endTime.toString)
  w.write("\n")
  w.write("Min Diff ")
  w.write(minDif.toString)
  w.write("\n")
  w.write("Sec Diff ")
  w.write(secDiff.toString)
  w.close()
}
