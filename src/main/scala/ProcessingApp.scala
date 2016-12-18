import myController._
import myTypes.TextBox
import processing.core.PApplet
import processing.event.MouseEvent

import scala.util.{Failure, Success, Try}


class ProcessingApp extends PApplet {
  implicit val pApp: PApplet = this

  var inputFile: String = "C:/input.txt"
  var outputFile: String = (inputFile.split("/").dropRight(1).toList ++ List("output.txt")).mkString("/")
  var locationFile: String = "C:/location.txt"
  var neighbor = 0
  var exponentL = 0
  var exponentR = 0
  var exponent = 0f
  var interpolationStatus = ""
  var interpolationGood = 0
  var interpolationBad = 0

  val inputFileTextBox = TextBox(10, 50, 550, 30)
  val locationFileTextBox = TextBox(10, 250, 550, 30)
  val exponentLeftTextBox = TextBox(350, 300, 50, 30)
  val exponentRightTextBox = TextBox(420, 300, 50, 30)
  val neighborTextBox = TextBox(350, 350, 50, 30)
  val interpolateButton = TextBox(90, 505, 150, 30)

  val zero = 0
  val marginPadding = 10
  val maxColor = 255

  override def settings(): Unit = {
    size(700, 600)
  }

  override def setup(): Unit = {
    textSize(24)
  }

  //This is an event loop
  override def draw(): Unit = {
    background(maxColor)

    showTextBox(inputFileTextBox)
    showTextBox(locationFileTextBox)
    showTextBox(exponentLeftTextBox)
    showTextBox(exponentRightTextBox)
    showTextBox(neighborTextBox)

    stroke(interpolationBad, interpolationGood, zero)
    showButton(interpolateButton.x, interpolateButton.y, interpolateButton.width, interpolateButton.height)

    fill(zero)
    stroke(zero, zero, zero)

    text("Location and File Name of the input data set:", marginPadding, inputFileTextBox.y - 50, width, height)
    text(inputFile, marginPadding, 50, width, height)

    text("Output will be written to file output.txt, Directory path: ", marginPadding, 100, width, height)
    text(outputFile, marginPadding, 150, width, height)

    text("Location and File Name of the interpolation points:", locationFileTextBox.x - 2, locationFileTextBox.y - 50, width, height)
    text(locationFile, locationFileTextBox.x, locationFileTextBox.y, width, height)

    text("Enter exponent value:", marginPadding, exponentLeftTextBox.y, width, height)
    text(s"$exponentL", exponentLeftTextBox.x, exponentLeftTextBox.y, width, height)
    text(".", exponentLeftTextBox.x + 55, exponentLeftTextBox.y, width, height)

    text(s"$exponentR", exponentRightTextBox.x, exponentRightTextBox.y, width, height)
    text("=", exponentRightTextBox.x + 125, exponentRightTextBox.y, width, height)
    text(s"$exponent", exponentRightTextBox.x + 145, exponentRightTextBox.y, width, height)

    text("Enter number of neighbors:", marginPadding, neighborTextBox.y, width, height)
    text(s"$neighbor", neighborTextBox.x, neighborTextBox.y, width, height)

    text("Interpolate!", interpolateButton.x + marginPadding, interpolateButton.y, width, height)
    text(interpolationStatus, marginPadding, 400, width, height)

    textSize(14)
    text("Note: Move mouse pointer inside a the boxes to type text, press the interpolate button ", marginPadding, 550, width, height)
    text("to perform IDW with time extension  (c) 2016 - James Hagans", marginPadding, 575, width, height)
    textSize(24)
  }


  override def keyPressed(): Unit = {
    if (mouseOverInputTextBox(inputFileTextBox)) {
      val (i, o) = inputOutputFiles(inputFile, outputFile)
      inputFile = i
      outputFile = o
    }

    if (mouseOverInputTextBox(locationFileTextBox)) {
      locationFile = inputLocationFile(locationFile)
    }

    if (mouseOverInputTextBox(exponentLeftTextBox)) {
      Try(inputNumber(s"$exponentL").toInt) match {
        case Success(leftOfDecimal) =>
          exponentL = leftOfDecimal
          exponent = s"$exponentL.$exponentR".toFloat
          interpolationStatus = ""
          interpolationGood = maxColor
          interpolationBad = zero
        case Failure(exception) =>
          exponentL = zero
          interpolationStatus = s"Can not use input as exponent $exception"
          interpolationBad = maxColor
          interpolationGood = zero
      }
    } else {

      if (mouseOverInputTextBox(exponentRightTextBox)) {
        Try(inputNumber(s"$exponentR").toInt) match {
          case Success(rightOfDecimal) =>
            exponentR = rightOfDecimal
            exponent = s"$exponentL.$exponentR".toFloat
            interpolationStatus = ""
            interpolationGood = maxColor
            interpolationBad = zero
          case Failure(exception) =>
            exponentR = zero
            interpolationStatus = s"Can not use input as exponent $exception"
            interpolationBad = maxColor
            interpolationGood = zero
        }
      } else {

        if (mouseOverInputTextBox(neighborTextBox)) {
          Try(inputNumber(s"$neighbor").toInt) match {
            case Success(i) =>
              neighbor = i
              interpolationStatus = ""
              interpolationGood = maxColor
              interpolationBad = zero
            case Failure(exception) =>
              neighbor = zero
              interpolationStatus = s"Can not use input as neighbor $exception"
              interpolationBad = maxColor
              interpolationGood = zero
          }
        } else {
          interpolationStatus = ""
          interpolationGood = zero
          interpolationBad = zero
        }
      }
    }

  }


  override def mouseClicked(event: MouseEvent): Unit = {
    if (mouseOverInputTextBox(interpolateButton)) {
      Try(interpolation(inputFile, outputFile, locationFile, neighbor, exponent)) match {
        case Success(_) =>
          interpolationStatus = s"Successfully Interpolated Files $inputFile and $locationFile"
          interpolationGood = 255
          interpolationBad = 0
        case Failure(exception) =>
          interpolationStatus = s"Could not Interpolate because $exception"
          interpolationBad = 255
          interpolationGood = 0
      }
    }
  }

  override def mousePressed(): Unit = {
    if (mouseOverInputTextBox(interpolateButton)) {
      interpolationStatus = s"Processing the Interpolation of Files $inputFile and $locationFile"
    }
  }

}

object ProcessingApp extends App {
  PApplet.main(Array[String]("ProcessingApp"))
}
