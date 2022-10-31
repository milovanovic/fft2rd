package fft2rd

import chisel3._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.Parameters

import chisel3.iotesters.PeekPokeTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import breeze.signal._
import breeze.linalg._
import scala.util.{Random}

class FFT2ControlWithPingPongTester (
  dut: AXI4StreamFFT2ControlBlock with AXI4FFT2ControlStandaloneBlock,
  params: FFT2ControlParams,
  beatBytes: Int = 4,
) extends PeekPokeTester(dut.module) with AXI4MasterModel {

  override def memAXI: AXI4Bundle = dut.ioMem.get
  val mod     = dut.module
  val radarDataCubeSize = params.rangeFFTSize*params.dopplerFFTSize*params.numTxs*params.numRxs
  val radarMatrixSize = params.rangeFFTSize*params.dopplerFFTSize*params.numTxs
  //Random.setSeed(11110L)

  val inData = Seq.fill(radarDataCubeSize)(Random.nextInt(1<<(beatBytes)).toDouble)
  val inRadarCube = Array.ofDim[Int](params.rangeFFTSize, params.dopplerFFTSize, params.numTxs, params.numRxs)

  var expected : Seq[Int] = Seq()
 // Random.setSeed(11110L)
  var cntIn = 0

  // move this to function or to utils class
  for (r <- 0 until params.numRxs) {
    for (y <- 0 until params.dopplerFFTSize) {
      for (z <- 0 until params.numTxs) {
        for (x <- 0 until params.rangeFFTSize) {
          inRadarCube(x)(y)(z)(r) = inData(cntIn).toInt
          cntIn = cntIn + 1
        }
      }
    }
  }
  cntIn = 0

  if (params.outputNodes == 1) {
    if (params.readXYZorXZY.get) {
    //  println("Define expected results:")
      for (z <- 0 until params.numTxs) {
        for (r <- 0 until params.numRxs) {
          for (x <- 0 until params.rangeFFTSize) {
            for (y <- 0 until params.dopplerFFTSize) {
              expected = expected :+ inRadarCube(x)(y)(z)(r) // perhaps whole sequence of data can be added
            }
          }
        }
      }
    }
    else {
      for (x <- 0 until params.rangeFFTSize) {
        for (z <- 0 until params.numTxs) {
          for (r <- 0 until params.numRxs) {
            for (y <- 0 until params.dopplerFFTSize) {
              expected = expected :+ inRadarCube(x)(y)(z)(r) // perhaps whole sequence of data can be added
            }
          }
        }
      }
    }
  }
  else if (params.outputNodes == params.numRxs) {
    for (z <- 0 until params.numTxs) {
      for (x <- 0 until params.rangeFFTSize) {
        for (y <- 0 until params.dopplerFFTSize) {
          for (r <- 0 until params.numRxs) {
            expected = expected :+ inRadarCube(x)(y)(z)(r) // perhaps whole sequence of data can be added
          }
        }
      }
    }
  }
  else if (params.outputNodes == params.numRxs * params.numTxs) {
    for (x <- 0 until params.rangeFFTSize) {
      for (y <- 0 until params.dopplerFFTSize) {
        for (z <- 0 until params.numTxs) {
          for (r <- 0 until params.numRxs) {
            expected = expected :+ inRadarCube(x)(y)(z)(r) // perhaps whole sequence of data can be added
          }
        }
      }
    }
  }

  var cntOut = 0
  var inValid = 0
  var cntInLoop = 0
  var outReady = 0
  var cntValidOut = 0
  val test = expected.map(_ + 1)
  var expectedFull = expected ++ test
  println(expectedFull(0).toString)
  println(expectedFull(1).toString)

  println("Expected data are:")
  expectedFull.foreach { c => println(c.toString) }
  println("end")

  while (!expectedFull.isEmpty) {
    if (cntInLoop < 2) {
      inValid = Random.nextInt(2)
      dut.ins.zipWithIndex.map { case (in, idx) =>
        poke(in.valid, inValid)
        if (peek(in.ready) == BigInt(1) && peek(in.valid) == BigInt(1)) {
          if (cntInLoop < 1) {
            poke(in.bits.data, inData(cntIn + idx*radarMatrixSize).toInt)
          }
          else {
            poke(in.bits.data, inData(cntIn + idx*radarMatrixSize).toInt + 1)
          }
        }
      }
      val readyIns = dut.ins.map { case in => if (peek(in.ready) == BigInt(1)) true else false }
      val readyAND = readyIns.reduce(_ && _)
      if (readyAND && inValid == 1) {
        cntIn = cntIn + 1
      }
      if (cntIn == params.rangeFFTSize * params.dopplerFFTSize * params.numTxs) {
        cntInLoop = cntInLoop + 1
        cntIn = 0
      }
    }
    else {
      dut.ins.foreach { in =>
        poke(in.valid, BigInt(0))
      }
    }
    outReady = Random.nextInt(2)
    dut.outs.zipWithIndex.map { case (out, idx) =>
      poke(out.ready, outReady)
      if (peek(out.ready) == BigInt(1) && peek(out.valid) == BigInt(1)) {
        expect(out.bits.data, expectedFull.head)
        /*if (expectedFull.length <= params.outputNodes) {
          expect(out.bits.last, 1)
        }*/
        expectedFull = expectedFull.tail
      }
    }
    step(1)
  }
  step(100)
}

//println("Output data:")
//println(peek(dut.outs(0).bits.data).toString)
//println("Expected data:")
//println(expected.head.toString)

class FFT2ControlWithPingPongSpec extends AnyFlatSpec with Matchers {
  val beatBytes = 4
  implicit val p: Parameters = Parameters.empty

  for (numRxs <- Seq(4)) {
    for (numTxs <- Seq(3)) {
      for (rangeFFTSize <- Seq(256)) {
        for (dopplerFFTSize <- Seq(32)) {
          for ((outputNodes, index) <- Seq(1, numRxs, numRxs*numTxs).zipWithIndex) {
            if (outputNodes == 1) {
              for (readXYZorXZY <- Seq(true)) {
                it should f"work for rangeFFTSize = $rangeFFTSize, dopplerFFTSize = $dopplerFFTSize, numRxs = $numRxs, numTxs = $numTxs, readXYZorXZY = $readXYZorXZY, outputNodes is equal to $outputNodes, with ping-pong" in {
                  val paramsFFT2Control: FFT2ControlParams = FFT2ControlParams(
                    rangeFFTSize = rangeFFTSize,
                    dopplerFFTSize = dopplerFFTSize,
                    pingPong = true,
                    numRxs = numRxs,
                    numTxs = numTxs,
                    outputNodes = outputNodes,
                    readXYZorXZY = Some(readXYZorXZY))
                  val testModule = LazyModule(new AXI4StreamFFT2ControlBlock(
                                                    paramsFFT2Control,
                                                    AddressSet(0x00000, 0xFF),
                                                    beatBytes = 4) with AXI4FFT2ControlStandaloneBlock)
                  chisel3.iotesters.Driver.execute(Array("verilator"), () => testModule.module) {
                          c => new FFT2ControlWithPingPongTester(dut = testModule,
                                    beatBytes = 4,
                                    params = paramsFFT2Control)}  should be (true)
                }
              }
            }
            else {
              it should f"work for rangeFFTSize = $rangeFFTSize, dopplerFFTSize = $dopplerFFTSize, numRxs = $numRxs, numTxs = $numTxs, outputNodes is equal to $outputNodes, with ping-pong ($index) " in {
                val paramsFFT2Control: FFT2ControlParams = FFT2ControlParams(
                  rangeFFTSize = rangeFFTSize,
                  dopplerFFTSize = dopplerFFTSize,
                  pingPong = true,
                  numRxs = numRxs,
                  numTxs = numTxs,
                  outputNodes = outputNodes,
                  readXYZorXZY = None)
                val testModule = LazyModule(new AXI4StreamFFT2ControlBlock(
                                                  paramsFFT2Control,
                                                  AddressSet(0x00000, 0xFF),
                                                  beatBytes = 4) with AXI4FFT2ControlStandaloneBlock)
                chisel3.iotesters.Driver.execute(Array("verilator"), () => testModule.module) {
                        c => new FFT2ControlWithPingPongTester(dut = testModule,
                                  beatBytes = 4,
                                  params = paramsFFT2Control)}  should be (true)
              }
            }
          }
        }
      }
    }
  }
}
