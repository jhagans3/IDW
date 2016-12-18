import myIO._
import myMath._
import myTypes._
import java.time.LocalDate
import processing.core.PApplet
import processing.core.PConstants._
import java.io.{BufferedWriter, FileWriter}



object myController {

  def showButton(x: Int, y: Int, w: Int, h: Int)(implicit p: PApplet): Unit = {
    p.fill(255)
    p.rect(x, y, w, h, 7)
  }

  def mouseOverInputTextBox(textBox: TextBox)(implicit p: PApplet): Boolean = {
    p.mouseX >= textBox.x && p.mouseX <= textBox.x+textBox.width && p.mouseY >= textBox.y && p.mouseY <= textBox.y+textBox.height
  }

  def inputOutputFiles(inputDir: String, outputDir: String)(implicit p: PApplet): (String, String) = {
    p.fill(0)
    if (p.keyCode == BACKSPACE) {
      if (inputDir.length > 0) {
        (inputDir.substring(0, inputDir.length - 1),
          (inputDir.split("/").dropRight(1).toList ++ List("output.txt")).mkString("/") )
      } else ("", outputDir)
    } else {
      if (p.keyCode != SHIFT && p.keyCode != CONTROL && p.keyCode != ALT) {
        (inputDir + p.key,
          (inputDir.split("/").dropRight(1).toList ++ List("output.txt")).mkString("/") )
      } else (inputDir, outputDir)
    }
  }

  def inputLocationFile(inputDir: String)(implicit p: PApplet): String = {
    p.fill(0)
    if (p.keyCode == BACKSPACE) {
      if (inputDir.length > 0) {
        inputDir.substring(0, inputDir.length - 1)
      } else ""
    } else {
      if (p.keyCode != SHIFT && p.keyCode != CONTROL && p.keyCode != ALT) {
        inputDir + p.key
      } else inputDir
    }
  }

  def inputNumber(inputNumber: String)(implicit p: PApplet): String = {
    p.fill(0)
    if (p.keyCode == BACKSPACE) {
      if (inputNumber.length > 0) {
        inputNumber.substring(0, inputNumber.length - 1)
      } else ""
    } else {
      if (p.keyCode != SHIFT && p.keyCode != CONTROL && p.keyCode != ALT) {
        inputNumber + p.key
      } else inputNumber
    }
  }


  def interpolation(inputFile: String, outputFile: String, locFile: String, neighbors: Int = 7, exponent: Float = 5f): Unit = {
    implicit val fileMap: MapPM25Values = file2Map(inputFile)
    implicit val radiusForDistance: Float = 2f
    val locations: MapLocationValues = locationFile2Map(locFile)
    val totalPoints = locations.size

    var counter = 0

    val extension = locations.par map { case (location, idValues) =>
      val id: Long = idValues.idNumber
      val date: LocalDate = LocalDate.ofYearDay(2009, location.t.toInt)
      val year: Int = date.getYear
      val month: Int = date.getMonthValue
      val day: Int = date.getDayOfMonth
      val interpolation: Double = location idwExtensionC(neighbors, exponent)

      counter = counter + 1
      println(s"Point $counter of $totalPoints")
      print(s"Output Data for $outputFile - ")
      println(id, year, month, day, interpolation)
      println
      (id, year, month, day, interpolation)
    }

    val header = "(county_id, year, month, day, pm25) \n"
    val w = new BufferedWriter(new FileWriter(outputFile))

    w.write(header)
    extension foreach { interpolation =>
      w.write(s"$interpolation\n")
    }
    w.close()

  }


  def showTextBox(textBox: TextBox)(implicit p: PApplet): Unit = {
    val toggleColor = {
      if (mouseOverInputTextBox(textBox)) 255
      else 0
    }

    p.stroke(0, 0, toggleColor)
    p.fill(255)
    p.rect(textBox.x, textBox.y, textBox.width, textBox.height)
  }

}
