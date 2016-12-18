import myIO._
import myMath._
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import myTypes.{KFoldErrors, MapPM25Values}
import java.io.{BufferedWriter, FileWriter}

object CVTest extends App {

  val k = 10
  val baseDir = "input/cv/fold"
  val numberOfFolds = (1 to k).toVector
  val numberOfNeighbors = (3 to 7).toVector
  val numberOfExponents = (1D to 5D by .5).toVector
  val fortyFive = for (f <- numberOfFolds; n <- numberOfNeighbors; e <- numberOfExponents) yield (f, n, e)
  val totalRuns = fortyFive.size

  val samplePoints = "st_sample.txt"
  val sampleValues = "value_sample.txt"
  val testPoints = "st_test.txt"
  val testValues = "value_test.txt"

  val startTime = LocalDateTime.now()
  println(startTime)
  val sampleVector: Vector[MapPM25Values] = generateTrainingData(numberOfFolds)(baseDir, samplePoints, sampleValues)

  val errors = fortyFive map { case (foldToLeaveOut, neighbors, exponent) =>

    val foldWriter = new BufferedWriter(new FileWriter(s"output/$neighbors-$exponent-${k}foldcv_idw_fold$foldToLeaveOut.txt"))

    val fold = kFold(k, foldToLeaveOut, neighbors, exponent.toFloat)(baseDir, sampleVector, testPoints)
    fold foreach {
      case (point, pmV) =>
        foldWriter.write(s"Neighbor: $neighbors, Exponent: $exponent, Point X: ${point.x}, Y: ${point.y}, Time: ${point.t}, Year: ${pmV.year}, Month ${pmV.month}, Day ${pmV.day}, PM 25 measurement:${pmV.measurement}")
        foldWriter.write("\n")
    }

    foldWriter.close()
    println(s"done with kFold ftlo $foldToLeaveOut")

    val errors: KFoldErrors = kFoldsError(foldToLeaveOut, neighbors, exponent)(baseDir, testPoints, testValues, fold)
    println(s"fold left out: ${errors.foldNotUsed}", errors.mae, errors.mse, errors.rmse, errors.mare, errors.msre, errors.rmsre)
    errors
  }

  val errorWriter = new BufferedWriter(new FileWriter(s"output/error_statistics_idw.txt"))
  val sumErrors: KFoldErrors = (errors foldLeft KFoldErrors(k, 0, 0D, 0D, 0D, 0D, 0D, 0D, 0D)) {
    (acc, error) =>
      val eMAE = acc.mae + error.mae
      val eMSE = acc.mse + error.mse
      val eRMSE = acc.rmse + error.rmse
      val eMARE = acc.mare + error.mare
      val eMSRE = acc.msre + error.msre
      val eRMSRE = acc.rmsre + error.rmsre
      errorWriter.write(s"MAE ${error.mae} for fold ${error.foldNotUsed} neighbors ${error.neighbors} exponent ${error.exponent}")
      errorWriter.write("\n")
      errorWriter.write(s"MSE ${error.mse} for fold ${error.foldNotUsed} neighbors ${error.neighbors} exponent ${error.exponent}")
      errorWriter.write("\n")
      errorWriter.write(s"RMSE ${error.rmse} for fold ${error.foldNotUsed} neighbors ${error.neighbors} exponent ${error.exponent}")
      errorWriter.write("\n")
      errorWriter.write(s"MSRE ${error.msre} for fold ${error.foldNotUsed} neighbors ${error.neighbors} exponent ${error.exponent}")
      errorWriter.write("\n")
      errorWriter.write(s"MARE ${error.mare} for fold ${error.foldNotUsed} neighbors ${error.neighbors} exponent ${error.exponent}")
      errorWriter.write("\n")
      errorWriter.write(s"RMSRE ${error.rmsre} for fold ${error.foldNotUsed} neighbors ${error.neighbors} exponent ${error.exponent}")
      errorWriter.write("\n")
      errorWriter.write("\n")
      KFoldErrors(k, error.neighbors, error.exponent, eMAE, eMSE, eRMSE, eMARE, eMSRE, eRMSRE)
  }

  val avgMAE = sumErrors.mae/totalRuns
  val avgMSE = sumErrors.mse/totalRuns
  val avgRMSE = sumErrors.rmse/totalRuns
  val avgMARE = sumErrors.mare/totalRuns
  val avgMSRE = sumErrors.msre/totalRuns
  val avgRMSRE = sumErrors.rmsre/totalRuns

  errorWriter.write("\n")
  errorWriter.write(s"average MAE $avgMAE")
  errorWriter.write("\n")
  errorWriter.write(s"average MSE $avgMSE")
  errorWriter.write("\n")
  errorWriter.write(s"average RMSE $avgRMSE")
  errorWriter.write("\n")
  errorWriter.write(s"average MSRE $avgMSRE")
  errorWriter.write("\n")
  errorWriter.write(s"average MARE $avgMARE")
  errorWriter.write("\n")
  errorWriter.write(s"average RMSRE $avgRMSRE")
  errorWriter.close()

  println(avgMAE)
  println(avgMSE)
  println(avgRMSE)
  println(avgMSRE)
  println(avgMARE)
  println(avgRMSRE)

  val endTime = LocalDateTime.now()
  val minDif =  ChronoUnit.MINUTES.between(startTime, endTime)
  val secDiff = ChronoUnit.SECONDS.between(startTime, endTime)
  println(endTime)
  println(minDif)
  println(secDiff)

}
