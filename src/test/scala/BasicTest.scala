import myIO._
import myMath._
import myTypes._

object BasicTest extends App {
  //file to map
  val fileMap: MapPM25Values = file2Map("pm25.txt")
    println(fileMap.size)
    println
    println


  //https://www.e-education.psu.edu/geog486/node/1877
  //idw
  implicit val idwMap: MapPM25Values = scala.collection.mutable.Map[XYT, PM25Values]()
  idwMap(XYT(1, 0)) = PM25Values(1, 34, 1, 1, 1)
  idwMap(XYT(2, 0)) = PM25Values(2, 33, 1, 1, 1)
  idwMap(XYT(2.5f, 0)) = PM25Values(25, 27, 1, 1, 1)
  idwMap(XYT(3, 0)) = PM25Values(3, 30, 1, 1, 1)
  idwMap(XYT(4, 0)) = PM25Values(4, 22, 1, 1, 1)
  idwMap(XYT(11, 0)) = PM25Values(11, 34, 1, 1, 1)

  val zeroZero: XYT = XYT(0,0)
  implicit val distanceFun: (XYT) => Double = zeroZero.euclideanDistance2
  val ec: (XYT, Double) => Double = zeroZero.euclideanDistance3c
  val extension: Double = zeroZero idwExtension(5, 2)
//  val extension: Double = zeroZero.idwExtension(5, 2f)(fileMap, distanceFun)
  val extensionC: Double = zeroZero.idwExtensionC(5, 2f)(fileMap, 100)
  println(extensionC)
  val wiki: Double = zeroZero idwWikipedia(5, 2)


  //32.380634976319946
//  println(extension)
//  println(wiki)



  //https://www.docear.org/2012/09/21/evaluations-in-information-retrieval-click-through-rate-ctr-vs-mean-absolute-error-mae-vs-root-mean-squared-error-mse-rmse-vs-precision/
  val estimate = List(1d, 1d, 1d, 1d, 1d, 1d, 1d, 1d, 1d, 1d)
  val actual = List(0d, 0d, 1d, 0d, 1d, 1d, 0d, 0d, 1d, 0d)

  val z = estimate zip actual

  //Mean Absolute Error
  //0.6
  println(mae(z))

  //Root Mean Squared Error
  //0.7745966692414834
  println(rmse(z))


}
