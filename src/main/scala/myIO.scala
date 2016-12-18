import myMath._
import myTypes._
import scala.io.Source._
import java.time.LocalDate
import scala.collection.mutable

object myIO {

  /** Takes a file with data in the shape of
    * (id, time, x, y, measurement) and converts
    * it to a map
    *
    * Will detect shape of time no need for user input
    *
    * @param fileName example file pm25_2009_measured.txt
    * @return map with the shape (XYT -> pm25value)
    */
  def file2Map(fileName: String): MapPM25Values = {
    val headerLine = 1
    val file: Vector[String] = (fromFile(name = fileName).getLines drop headerLine).toVector
    val res = mutable.Map[XYT, PM25Values]()

    file map {
      x => x.split("\t")
    } map {
      array => {
        array.length match {
          case 5 =>
            val id = array(0).toInt
            val year = array(1).toInt
            val month = 1
            val day = 1
            val t = LocalDate.of(year, month, day).getDayOfYear
            val point = XYT(array(2).toFloat, array(3).toFloat, t)
            val measurement = array(4).toFloat
            (point, PM25Values(id, measurement, year, month, day))

          case 6 =>
            val id = array(0).toInt
            val year = array(1).toInt
            val month = array(2).toInt
            val day = 1
            val t = LocalDate.of(year, month, day).getDayOfYear
            val point = XYT(array(3).toFloat, array(4).toFloat, t)
            val measurement = array(5).toFloat
            (point, PM25Values(id, measurement, year, month, day))

          case 7 =>
            val id = array(0).toInt
            val year = array(1).toInt
            val month = array(2).toInt
            val day = array(3).toInt
            val t = LocalDate.of(year, month, day).getDayOfYear
            val point = XYT(array(4).toFloat, array(5).toFloat, t)
            val measurement = array(6).toFloat
            (point, PM25Values(id, measurement, year, month, day))

          case _ =>
            val id = 0
            val year = 2009
            val month = 1
            val day = 1
            val t = LocalDate.of(year, month, day).getDayOfYear
            val point = XYT(0, 0, t)
            val measurement = 0
            (point, PM25Values(id, measurement, year, month, day))
        }
      }
    } foreach {
      case (point, pmV) => res(point) = pmV
    }

    res
  }

  /** Takes a file of id x y data, and adds a time dimension
    *
    * @param fileName example county_xy.txt
    * @return a map in the shape of (XYT -> locationID)
    */
  def locationFile2Map(fileName: String): MapLocationValues = {
    val headerLine = 1
    val file: Iterator[String] = fromFile(name = fileName).getLines drop headerLine
    val res = mutable.Map[XYT, LocationValues]()

    file foreach { line =>
      val lineArray = line.split("\t")
      val id = lineArray(0).toLong

      //For one point all days of the year are created
      (1 to 365) foreach { day =>
        val xyt = XYT(lineArray(1).toFloat, lineArray(2).toFloat, day)
        res(xyt) = LocationValues(id)
      }
    }

    res
  }

  /** Takes a file of points and pm25 values
    * and concatenates them into a map
    *
    * @param pointsFile file name of file with points
    * @param valuesFile file name of file with pm25 values
    * @return map of the shape (point -> pm25value)
    */
  def concatPointsAndValuesFiles(pointsFile: String, valuesFile: String): MapPM25Values = {
    val pointsFromFile = fromFile(name = pointsFile).getLines
    val valuesFromFile = fromFile(name = valuesFile).getLines
    val zipped = pointsFromFile zip valuesFromFile
    val res = mutable.Map[XYT, PM25Values]()

    zipped foreach { case (pointLine, valueLine) =>
      val pointData = pointLine.split("\t")
      val x = pointData(0).toFloat
      val y = pointData(1).toFloat
      val t = pointData(2).toFloat
      val pmV = valueLine.toFloat
      val xyt = XYT(x, y, t)
      val (year, month, day) = xyt.t2YYYYMMDD

      res(xyt) = PM25Values(0, pmV, year, month, day)
    }

    res
  }

  /** takes a file of points and converts
    * the points to a vector of XYT points
    *
    * @param fileName file with points
    * @return XYT points
    */
  def xytFile2Map(fileName: String): XYTs = {
    val file: Vector[String] = fromFile(name = fileName).getLines.toVector

    val res: XYTs = file map { line =>
      val lineArray = line.split("\t")
      XYT(lineArray(0).toFloat, lineArray(1).toFloat, lineArray(2).toFloat)
    }

    res
  }

  /**
    *
    * @param oneToK the number of folds 1 to k
    * @param baseDir file base dir
    * @param sampleDataPointsFileName file with points x,y,t
    * @param sampleDataPM25ValuesFileName file with the vales for the point
    * @return vector with a map with the shap (point -> value)
    */
  def generateTrainingData(oneToK: Vector[Int])(baseDir: String, sampleDataPointsFileName: String, sampleDataPM25ValuesFileName: String): Vector[MapPM25Values] ={
    val sampleVector: Vector[MapPM25Values] = oneToK map { number =>
      val pointsFile = s"$baseDir$number/$sampleDataPointsFileName"
      val valuesFile = s"$baseDir$number/$sampleDataPM25ValuesFileName"
      concatPointsAndValuesFiles(pointsFile, valuesFile)
    }

    sampleVector

  }


}
