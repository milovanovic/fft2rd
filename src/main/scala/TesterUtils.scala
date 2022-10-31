// necessary libraries
package fft2rd

import breeze.math.Complex
import breeze.linalg._
import breeze.signal._

import scala.collection.mutable.ArrayBuffer
import smile.plot._
import smile.plot.swing._


class TesterUtils {
  /**
   * Generates complex or real sinusoids with optional noise
   */
  def getTone(numSamples: Int, f1r: Double, f2r: Double = 0, f1i: Double = 0, f2i: Double = 0, addNoise: Double = 0): Seq[Complex] = {
    require(f1r != 0, "Digital frequency should not be zero!")
    import scala.util.Random

    (0 until numSamples).map(i => Complex(
    (math.sin(2 * math.Pi * f1r * i) + math.sin(2 * math.Pi * f2r * i)) + addNoise*((Random.nextDouble()*2.0)-1.0)/64,
    (math.sin(2 * math.Pi * f1i * i) + math.sin(2 * math.Pi * f2i * i)) + addNoise*((Random.nextDouble()*2.0)-1.0)/64))
  }

  /**
  * Generate radar data matrix
  */
  def genSimpleInputTo2Dfft (rangeFFTSize: Int, dopplerFFTSize: Int, f1r: Double, f1i: Double = 0, f2r: Double = 0, f2i: Double = 0, addNoise: Double = 0): DenseMatrix[Complex] = {
    val denseMatrix = DenseMatrix.zeros[Complex](dopplerFFTSize, rangeFFTSize)
    // not elegant but fast solution
    for (i <- 0 until dopplerFFTSize) {
      val signal = getTone(rangeFFTSize, f1r, f1i, f2r, f2i, addNoise)
      for (j <- 0 until rangeFFTSize) {
        denseMatrix(i, j) = signal(j)
      }
    }
    denseMatrix
  }

  /**
  * Apply 2D fft on radar data matrix
  */
  def gen2Dfft(radarMatrix: DenseMatrix[Complex]): DenseMatrix[Complex] = {
    val fft2 = fourierTr(radarMatrix)
    fft2
  }

  /**
  * Do abs of all complex data inside range-doppler matrix
  */
  def genAbs2Dfft(rangeDopplerMatrix: DenseMatrix[Complex]): DenseMatrix[Double] = {
    //val abs2Dfft = rangeDopplerMatrix.map(c => c.abs()) - this does not work
    val rows = rangeDopplerMatrix.rows
    val cols = rangeDopplerMatrix.cols
    val abs2Dfft = DenseMatrix.zeros[Double](rows, cols)

    // not elegant but fast solution
    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        abs2Dfft(i, j) = rangeDopplerMatrix(i, j).abs
      }
    }
    abs2Dfft
  }

  /**
  * This function should just shift rows in specific form so that plot looks as usual representation of range-doppler matrix, it is assumed that magnitude is calculated
  */
  def fft2Shift(fft2Matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val rows = fft2Matrix.rows
    val cols = fft2Matrix.cols
    val denseMatrix = DenseMatrix.zeros[Double](rows, cols)

    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        if (i >= rows/2) {
          denseMatrix(i - rows/2, j) = fft2Matrix(i, j)
        }
        else {
          denseMatrix(i + rows/2, j) = fft2Matrix(i,j)
        }
      }
    }
    denseMatrix
  }

  /**
  * plot 2D fft with heatmap, if it is easier i guess that this absRangeDopplerMatrix can be converted to non DenseMatrix
  * it can have some additional parameters specific for plotting if that is necessary
  */

  def denseMatrix2Array2D(mat: DenseMatrix[Double]): Array[Array[Double]] = {
    val (n, m) = mat.keySet.max
    val arr2buf = ArrayBuffer[Array[Double]]()

    for (i <- 0 to n) {
      val arrbuf = ArrayBuffer[Double]()
      for(j <- 0 to m) {
        arrbuf += mat.valueAt(i, j)
      }
      arr2buf += arrbuf.toArray
    }
    arr2buf.toArray
  }

  //   should convert to dB
  def convertToLogDomain(mat: DenseMatrix[Double]): DenseMatrix[Double] = {
    val rows = mat.rows
    val cols = mat.cols
    val mat_dB = DenseMatrix.zeros[Double](rows, cols)

    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        mat_dB(i, j) = 20*scala.math.log10(mat(i, j))
      }
    }
    mat_dB
  }

  def plot2Dfft(absRangeDopplerMatrix: DenseMatrix[Double]) {
    val arr2 = denseMatrix2Array2D(absRangeDopplerMatrix)
    val max  = arr2.flatten.max
    val testHeatMap = heatmap(arr2, Palette.jet(256))
    testHeatMap.setTitle("Doppler-Range heatmap")
    testHeatMap.setAxisLabels("Range[m]", "Velocity [m/s]")
    show(testHeatMap)(Render.desktop)
  }
}

object TestUtilsFFT2App extends App {
  val testerUtils = new TesterUtils()
  val radarMatrix = testerUtils.genSimpleInputTo2Dfft(256, 42, 0.66, 0.45, 0.45, 0.68, 1) // just for test
  val fft2 = testerUtils.gen2Dfft(radarMatrix)
  val absFFT2 = testerUtils.genAbs2Dfft(fft2)
  val convertToLog = testerUtils.convertToLogDomain(absFFT2)
  val shiftedFFT2 = testerUtils.fft2Shift(absFFT2)
  testerUtils.plot2Dfft(absFFT2)
  testerUtils.plot2Dfft(shiftedFFT2)
  // println(absFFT2.toString)
  // here plot 2D fft should be called
}



