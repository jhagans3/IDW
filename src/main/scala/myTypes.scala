import scala.collection.mutable
import scala.collection.parallel.mutable.ParIterable

object myTypes {
  final case class XYT(x: Float, y: Float, t: Float = 0)
  final case class PM25Values(idNumber: Int, measurement: Double, year: Int, month: Int, day: Int)
  final case class LocationValues(idNumber: Long)


  final case class KFoldErrors(foldNotUsed: Int, neighbors: Int, exponent: Double, mae: Double, mse: Double, rmse: Double, mare: Double, msre: Double, rmsre: Double)

  type XYTs =  Vector[XYT]
  type MapPM25Values =  scala.collection.mutable.Map[XYT, PM25Values]
  type MapLocationValues =  scala.collection.mutable.Map[XYT, LocationValues]
  type IFormatForFile = ParIterable[(Long, Int, Int, Int, Double)]

  val emptyPM25Map: MapPM25Values = mutable.Map[XYT, PM25Values]()
  val zeroBasedPM25Map: MapPM25Values = mutable.Map[XYT, PM25Values](XYT(0,0) -> PM25Values(0, 0D, 0, 0, 0))

  final case class TextBox(x: Int, y: Int, width: Int, height: Int)

}
