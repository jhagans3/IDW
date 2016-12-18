import myIO._
import myTypes._
import scala.math._
import java.time.LocalDate

object myMath {

  /** Unknown PM25 value for point w
    *
    * @param xyt point w
    */
  implicit class XYTPoint(xyt: XYT) {

    /** function to find the c for the time dimension
      * for the distance function of idw's lambda
      *
      * @param points all the points in the sample space to find min and max
      * @return the constant value
      */
    def createTimeConstant(points: Vector[XYT]): Double = {
      val left = 1d/364d
      val xs = points map (p => p.x)
      val ys = points map (p => p.y)
      val xDiff = xs.max - xs.min
      val yDiff = ys.max - ys.min
      val right = (xDiff + yDiff) / 2

      left * right
    }

    /** Function used to convert a XYT point's T
      * dimension from the day of the year to the
      * date format of year month day
      *
      * @return tuple3 of (year, month, day)
      */
    def t2YYYYMMDD: (Int, Int, Int) = {
      val date: LocalDate = LocalDate.ofYearDay(2009, xyt.t.toInt)
      val year: Int = date.getYear
      val month: Int = date.getMonthValue
      val day: Int = date.getDayOfMonth

      (year, month, day)
    }

    /** The distance between points w and wi on x and y dimension
      *
      * @param wi point to compare to w
      * @return the distance between w and wi
      */
    def euclideanDistance2(wi: XYT): Double = {
      val xs = wi.x - xyt.x
      val ys = wi.y - xyt.y

      val xs2 = pow(xs, 2)
      val ys2 = pow(ys, 2)

      val insideRadical = xs2 + ys2
      val res: Double = sqrt(insideRadical)

      res
    }

    /** The distance between points w and wi on x and y dimension
      *
      * @param wi point to compare to w
      * @param c the constant to multiply the t dimension with
      * @return the distance between w and wi
      */
    def euclideanDistance3c(wi: XYT, c: Double = 1D): Double = {
      val xs = wi.x - xyt.x
      val ys = wi.y - xyt.y
      val ts = wi.t - xyt.t

      val xs2 = pow(xs, 2)
      val ys2 = pow(ys, 2)
      val ts2 = pow(ts, 2)
      val c2 = pow(c, 2)

      val insideRadical = xs2 + ys2 + (c2 * ts2)
      val res: Double = sqrt(insideRadical)
      res
    }

    /** The distance between points w and wi on x and y dimension
      *
      * @param wi point to compare to w
      * @return the distance between w and wi
      */
    def euclideanDistance3(wi: XYT): Double = {
      val xs = wi.x - xyt.x
      val ys = wi.y - xyt.y
      val ts = wi.t - xyt.t

      val xs2 = pow(xs, 2)
      val ys2 = pow(ys, 2)
      val ts2 = pow(ts, 2)

      val insideRadical = xs2 + ys2 + ts2
      val res: Double = sqrt(insideRadical)

      res
    }

    /** The function to find the weight for the know w values
      *
      * @param wi the point to compare to w
      * @param p the exponent value
      * @param neighbors the neighbors of point w
      * @param distanceFunction function to find the distance between w and wi
      * @return
      */
    def lambda(wi: XYT, p: Float, neighbors: XYTs,
               distanceFunction: (XYT) => Double): Double = {

      val numerator = pow(1/distanceFunction(wi), p)
      val denominatorSummation = (neighbors foldLeft 0d) {
        (summation, k) => summation + pow(1/distanceFunction(k), p)
      }
      val res: Double = numerator/denominatorSummation
      res
    }

    /** The function to find the weight for the know Wi values
      *
      * @param wi a point with a known pm25 value
      * @param p the exponent that influences the weight of the value at Wi
      * @param neighbors the Wi's that are used for interpolation of W
      * @param c the constant for the distance function's time dimension
      * @param distanceFunction the Euclidean function for x y ct
      * @return the weight for Wi's pm25 value
      */
    def lambdaC(wi: XYT, p: Float, neighbors: XYTs, c: Double,
               distanceFunction: (XYT, Double) => Double): Double = {

      val d = distanceFunction(wi, c)
      val numerator = pow(1/d, p)
      val denominatorSummation: Double = (neighbors.par foldLeft 0d) {
        (summation, k) => {
          val dd = distanceFunction(k, c)
          summation + pow(1/dd, p)
        }
      }

//optimize?
//      var denominatorSummation: Double = 0d
//      neighbors.par foreach {
//        k =>
//          val dd = distanceFunction(k, c)
//          denominatorSummation = denominatorSummation + pow(1/dd, p)
//      }

      val res: Double = numerator/denominatorSummation
//      println(s"      $numerator / $denominatorSummation  $d")
      res
    }


    /** Find number of neighbors of w from sampleSpace within the search radius
      *
      * @param number the number of neighbors of point w
      * @param searchRadius the radius of the circle around w to find neighbors
      * @param sampleSpace  the set of points to look for neighbors
      * @return
      */
    def findNeighbors(number: Int, searchRadius: Float, sampleSpace: XYTs): Vector[XYT] = {
//optimize?
      val filteredPoints = sampleSpace.par flatMap {
        point => {
          val xs = point.x - xyt.x
          val ys = point.y - xyt.y

          val xs2 = pow(xs, 2)
          val ys2 = pow(ys, 2)

          val euclidean = xyt.euclideanDistance3(point)
          val d = xs2 + ys2
          if (d <= pow(searchRadius, 2) && euclidean != 0) Some((point, d))
          else None
        }
      }

      val sortedPoints = filteredPoints.toVector sortWith {
        case ((_, distance1), (_, distance2)) => distance1 < distance2
      }

      val neighbors: Vector[XYT] = (sortedPoints take number) map { case (point, _) => point }
      neighbors
    }

    /** Function for Inverse Distance Weighting for point w
      *
      * @param numberOfNeighbors the number of neighbors to weigh against
      * @param exponent the exponent
      * @param mapOfPMValues the points for finding the interpolation of w's value
      * @param distanceFunction function to find the distance between w and wi
      * @return
      */
    def idwExtension(numberOfNeighbors: Int, exponent: Float)
                    (implicit mapOfPMValues: MapPM25Values,
                     distanceFunction: (XYT) => Double): Double = {

      val sampleSpace: Vector[XYT] = mapOfPMValues.keys.toVector
      val neighbors = xyt.findNeighbors(numberOfNeighbors, 10, sampleSpace)

      val predictedValue: Double = (neighbors foldLeft 0d) {
        (summation, Wi) => {
          val weight: Double = xyt.lambda(Wi, exponent, neighbors, distanceFunction)
          val valueAtWi: Double = mapOfPMValues(Wi).measurement
          summation + (valueAtWi * weight)
        }
      }

      predictedValue
    }

    /** Implementation of Inverse Distance Weighting
      * with a constant in the distance function
      * see section 2.2.1 of M8 paper to Review
      *
      * @param numberOfNeighbors number of neighbors to use for interpolation
      * @param exponent the exponent used to find the weight of a pm25 value at Wi
      * @param mapOfPMValues space of points used for interpolation of point W
      * @param radius the search area of the points
      * @return IDW value
      */
    def idwExtensionC(numberOfNeighbors: Int, exponent: Float)
                     (implicit mapOfPMValues: MapPM25Values, radius: Float): Double = {
      val sampleSpace: XYTs = mapOfPMValues.keys.toVector
      val neighbors: XYTs = xyt.findNeighbors(numberOfNeighbors, radius, sampleSpace)
      val constant: Double = createTimeConstant(sampleSpace)
      println(s"Exponent: $exponent Neighbors:${neighbors.size} of set: ${sampleSpace.size} c: $constant")

      val predictedValue: Double = (neighbors.par foldLeft 0d) {
        (summation, Wi) => {
          val weight: Double = xyt.lambdaC(Wi, exponent, neighbors, constant, euclideanDistance3c)
          val valueAtWi: Double = mapOfPMValues(Wi).measurement
          println("In summation of IDW extension")
          println(s"Wi: $Wi, weight: $weight, Wi value $valueAtWi, weight * Wi ${weight * valueAtWi} w $xyt")
          summation + (valueAtWi * weight)
        }
      }

      predictedValue

//optimize?
//      var predictedValue = 0d
//      neighbors.par foreach {
//        Wi => {
//          val weight: Double = xyt.lambdaC(Wi, exponent, neighbors, constant, euclideanDistance3c)
//          val valueAtWi: Double = mapOfPMValues(Wi).measurement
//          predictedValue = predictedValue + (valueAtWi * weight)
//        }
//      }
//
//      predictedValue
    }

    /** Shepard's method
      *
      * @param wi the point to compare distances
      * @param p the exponent
      * @param distanceFunction function to find the distance between w and wi
      * @return weight for value at Wi
      */
    def lambdaWikipedia(wi: XYT, p: Float,
                        distanceFunction: (XYT) => Double): Double = {
      val numerator = 1
      val denominator = pow(distanceFunction(wi), p)
      val res = numerator/denominator
      res
    }

    /** https://en.wikipedia.org/wiki/Inverse_distance_weighting
      *
      * @param numberOfNeighbors the number of neighbors to weigh against
      * @param exponent the exponent
      * @param mapOfPMValues the points for finding the interpolation of w's value
      * @param distanceFunction function to find the distance between w and wi
      * @return idw value
      */
    def idwWikipedia(numberOfNeighbors: Int, exponent: Float)
                    (implicit mapOfPMValues: MapPM25Values,
                     distanceFunction: (XYT) => Double): Double = {

      val sampleSpace = mapOfPMValues.keys.toVector
      val neighbors = xyt.findNeighbors(numberOfNeighbors, 10, sampleSpace)

      val numeratorSummation = (neighbors foldLeft 0d) {
        (summation, Wi) => {
          val weight = xyt.lambdaWikipedia(Wi, exponent, distanceFunction)
          val u = mapOfPMValues(Wi).measurement
          val wu = weight * u
          summation + wu
        }
      }

      val denominatorSummation = (neighbors foldLeft 0d) {
        (summation, Wi) => {
          val weight = xyt.lambdaWikipedia(Wi, exponent, distanceFunction)
          summation + weight
        }
      }

      numeratorSummation/denominatorSummation

    }

  }

  /** Mean Absolute Error
    *
    * @param xs List of (interpolated values, original values)
    * @return
    */
  def mae(xs: List[(Double, Double)]): Double = {
    val numerator = (xs.par foldLeft 0d) {
      (acc, pair) => {
        val (i, o) = pair
        acc + abs(i - o)
      }
    }
    val denominator = xs.size
    val res: Double = numerator/denominator
    res
  }

  /** Root Mean Squared Error
    *
    * @param xs List of (interpolated values, original values)
    * @return
    */
  def rmse(xs: List[(Double, Double)]): Double = {
    val numerator = (xs.par foldLeft 0d) {
      (acc, pair) => {
        val (i, o) = pair
        acc + pow(i - o, 2)
      }
    }
    val denominator = xs.size
    val res = sqrt(numerator/denominator)
    res
  }

  /** Mean Squared Error
    *
    * @param xs List of (interpolated values, original values)
    * @return
    */
  def mse(xs: List[(Double, Double)]): Double = {
    val numerator = (xs.par foldLeft 0d) {
      (acc, pair) => {
        val (i, o) = pair
        acc + pow(i - o, 2)
      }
    }
    val denominator = xs.size
    val res: Double = numerator/denominator
    res
  }

  /** Mean Absolute Relative Error
    *
    * @param xs List of (interpolated values, original values)
    * @return
    */
  def mare(xs: List[(Double, Double)]): Double = {
    val numerator = (xs.par foldLeft 0d) {
      (acc, pair) => {
        val (i, o) = pair
        val littleNumerator = abs(i - o)
        val littleDenominator = o
        val littleQ = littleNumerator / littleDenominator
        acc + littleQ
      }
    }
    val denominator = xs.size
    val res: Double = numerator/denominator
    res
  }

  /** Mean Square Relative Error
    *
    * @param xs List of (interpolated values, original values)
    * @return
    */
  def msre(xs: List[(Double, Double)]): Double = {
    val numerator = (xs.par foldLeft 0d) {
      (acc, pair) => {
        val (i, o) = pair
        val littleNumerator = pow(i - o, 2)
        val littleDenominator = o
        val littleQ = littleNumerator / littleDenominator
        acc + littleQ
      }
    }
    val denominator = xs.size
    val res: Double = numerator/denominator
    res
  }

  /** Root Mean Squared Relative Error
    *
    * @param xs List of (interpolated values, original values)
    * @return
    */
  def rmsre(xs: List[(Double, Double)]): Double = {
    val numerator = (xs.par foldLeft 0d) {
      (acc, pair) => {
        val (i, o) = pair
        val littleNumerator = pow(i - o, 2)
        val littleDenominator = o
        val littleQ = littleNumerator / littleDenominator
        acc + littleQ
      }
    }
    val denominator = xs.size
    val res: Double = sqrt(numerator/denominator)
    res
  }

  /** K fold cross validation
    *
    * @param k the k or the total number of folds
    * @param foldNotToUse the k left out of the k-folds
    * @param testDataPointsFileName file name for test points
    * @return Map with the shape (point -> interpolated pm25value)
    */
  def kFold(k: Int, foldNotToUse: Int, neighbor: Int, exponent: Float)(baseDir: String, sampleVector: Vector[MapPM25Values], testDataPointsFileName: String): MapPM25Values = {
    implicit val radiusForDistance = 2f
    val oneToK = (1 to k).toVector
    val kFoldsMinusOne: Vector[Int] = oneToK.diff(List(foldNotToUse))
//    val sampleVector: Vector[MapPM25Values] = generateTrainingData(oneToK)(baseDir, sampleDataPointsFileName, sampleDataPM25ValuesFileName)
    val sampleKFolds: Vector[MapPM25Values] = Vector(zeroBasedPM25Map) ++ sampleVector

    //zero ++ slice(0, foldNotToUse) ++ slice(foldNotToUse+1, k)?
    implicit var trainingData: MapPM25Values = zeroBasedPM25Map
    kFoldsMinusOne foreach { index =>
      trainingData = trainingData ++ sampleKFolds(index)
    }

    val kFoldsOfTestXYTs: XYTs =  xytFile2Map(s"$baseDir$foldNotToUse/$testDataPointsFileName")

    val foldInterpolation: MapPM25Values = emptyPM25Map
    foldInterpolation.clear()
    println(s" For fold $foldNotToUse Before ${foldInterpolation.size} n $neighbor, e $exponent")

    kFoldsOfTestXYTs.par foreach { xyt =>
      val (year, month, day) = xyt.t2YYYYMMDD
      val interpolation: Double = xyt.idwExtensionC(7, 5f)
      val pmV = PM25Values(0, interpolation, year, month, day)

//        println(s"interp on fold $foldNotToUse, point $xyt, w interp PMvalue $interpolation")
      foldInterpolation(xyt) = pmV
    }

//    println(s" For fold $foldNotToUse After ${foldInterpolation.size}")
    foldInterpolation
  }


  /** Errors of a k fold
    *
    * @param foldNotUsed the fold to test
    * @param neighbors the number of closest points
    * @param exponent the value for lambda
    * @param baseDir the directory of the fold files
    * @param testDataPointsFileName the filename of the test points
    * @param testDataPM25ValuesFileName filename of the values for the test data points
    * @param sampleMap the sample or training data
    * @return a class with error functions in the shape (mae, mse, rmse, mare, msre, rmsre)
    */
  def kFoldsError(foldNotUsed: Int, neighbors: Int, exponent: Double)(baseDir: String, testDataPointsFileName: String, testDataPM25ValuesFileName: String, sampleMap: MapPM25Values ): KFoldErrors = {

//    println(sampleMap.size)
    val testMap: MapPM25Values = concatPointsAndValuesFiles(s"$baseDir$foldNotUsed/$testDataPointsFileName", s"$baseDir$foldNotUsed/$testDataPM25ValuesFileName")
    val foldXYTs = sampleMap.keys.toList
//    println(testMap.size)

    val foldIOs: List[(Double, Double)] = foldXYTs map { xyt =>
      val o = testMap(xyt).measurement
      val i = sampleMap(xyt).measurement
//      println(s"    for fold $foldNotUsed point $xyt")

      (i, o)
    }

    val mymae = mae(foldIOs)
    val mymse = mse(foldIOs)
    val myrmse = rmse(foldIOs)
    val mymare = mare(foldIOs)
    val mymsre = msre(foldIOs)
    val myrmsre = rmsre(foldIOs)

    KFoldErrors(foldNotUsed, neighbors, exponent, mymae, mymse, myrmse, mymare, mymsre, myrmsre)
  }


}
