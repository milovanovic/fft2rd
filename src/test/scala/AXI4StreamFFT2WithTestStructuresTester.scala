package fft2rd

import chisel3._
import chisel3.util._
import chisel3.experimental._

import fft._
import zeropadder._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.Parameters

import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FlatSpec, Matchers}
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers

import breeze.math.Complex
import scala.util.{Random}
import scala.io.Source
import dsptools.numbers._

// Works ok when zeropadder is on on and when it is off
class AXI4StreamFFT2RDWithTestStructuresBlockTester (
  dut: AXI4StreamFFT2RDWithTestStructuresBlock with AXI4FFT2RDWithTestStructuresStandaloneBlock,
  params: FFT2RDTSParams,
  inFileNameReal: String,
  inFileNameImag: String,
  outFileNameReal: String,
  outFileNameImag: String,
  beatBytes: Int = 4,
  silentFail: Boolean = false,
) extends PeekPokeTester(dut.module) with AXI4MasterModel {

  def asNdigitBinary (source: Int, digits: Int): String = {
    val lstring = source.toBinaryString
    if (source >= 0) {
      val l: java.lang.Long = lstring.toLong
      String.format ("%0" + digits + "d", l)
    }
    else
      lstring.takeRight(digits)
  }

  /**
   * Format complex data to be compatible with AXI4 stream interface (lenght = 32)
   */
  def formAXI4StreamComplexData(inData : Seq[Complex], dataWidth: Int): Seq[Int] = {
    inData.map(data => java.lang.Long.parseLong(
                                  asNdigitBinary(data.real.toInt, dataWidth) ++
                                  asNdigitBinary(data.imag.toInt, dataWidth), 2).toInt)
  }

  def readFile(filename: String): Array[Int] = {
    val bufferedSource = Source.fromFile(filename)
    val contents = Source.fromFile(filename).getLines.toArray.map { br => br.toInt }
    bufferedSource.close
    contents
  }

  override def memAXI: AXI4Bundle = dut.ioMem.get
  val mod     = dut.module

  val inputDataReal = readFile(inFileNameReal)
  val inputDataImag = readFile(inFileNameImag)
  val outputDataReal = readFile(outFileNameReal)
  val outputDataImag = readFile(outFileNameImag)

  val inData = formAXI4StreamComplexData(inputDataReal.zip(inputDataImag).map { case(real, imag) => Complex(real.toInt, imag.toInt) }.toSeq, 16)     // form complex input data
  var expected = formAXI4StreamComplexData(outputDataReal.zip(outputDataImag).map { case(real, imag) => Complex(real.toInt, imag.toInt) }.toSeq, 16) // form complex output data
  val numberOfLoops = params.paramsFFT2RD.fft2ControlParams.rangeFFTSize*params.paramsFFT2RD.fft2ControlParams.dopplerFFTSize*params.paramsFFT2RD.fft2ControlParams.numTxs
  val totalOutData = numberOfLoops * params.paramsFFT2RD.fft2ControlParams.numRxs // this is all for number of outputNodes equal to 1

  var inValid = 0
  var cntIn = 0
  poke(dut.out.ready, 0)

 // Random.setSeed(11110L)
  while (cntIn < numberOfLoops) {
    inValid = Random.nextInt(2)
    dut.ins.zipWithIndex.map { case (in, idx) =>
      poke(in.valid, inValid)
      if (peek(in.ready) == BigInt(1) && peek(in.valid) == BigInt(1)) {
        poke(in.bits.data, inData(cntIn + idx*numberOfLoops).toInt)
        if (cntIn == (numberOfLoops - 1)) {
          poke(in.bits.last, BigInt(1))
        }
      }
    }
    val readyIns = dut.ins.map { case in => if (peek(in.ready) == BigInt(1)) true else false }
    val readyAND = readyIns.reduce(_ && _)
    step(1)
    if (readyAND && inValid == 1) {
      cntIn = cntIn + 1
    }
  }

  dut.ins.foreach { in =>
    poke(in.valid, BigInt(0))
  }

  dut.ins.foreach { in =>
    poke(in.bits.last, BigInt(0))
  }
  //Random.setSeed(11110L)
  cntIn = 0

  step (20)
  var cntValid = 0
  var peekedVal: BigInt = 0
  var outReady = 0
  var cntRawData = 0
  val expectedRawData = inData.take(numberOfLoops)

  if (params.paramsFFT2RD.fft2ControlParams.outputNodes == 1) {
     while (cntValid < totalOutData) {
      outReady = Random.nextInt(2)
      poke(dut.out.ready, outReady)
      if (peek(dut.out.ready) == BigInt(1) && peek(dut.out.valid) == BigInt(1)) {
        peekedVal = peek(dut.out.bits.data)
        assert(peekedVal.toInt == expected(cntValid))
        if (peekedVal.toInt == expected(cntValid)) {
          //println(cntValid.toString)
          cntValid = cntValid + 1
        }
       // cntValid = cntValid + 1
        if (cntValid == totalOutData) {
          expect(dut.out.bits.last, 1)
        }
      }
      step(1)
    }
    while (cntRawData < numberOfLoops) {
      outReady = Random.nextInt(2)
      poke(dut.out.ready, outReady)
      if (peek(dut.out.ready) == BigInt(1) && peek(dut.out.valid) == BigInt(1)) {
        peekedVal = peek(dut.out.bits.data)
        if (cntRawData == 0) {
          println(peekedVal.toString)
        }
        assert(peekedVal.toInt == expectedRawData(cntRawData))
        if (peekedVal.toInt == expectedRawData(cntRawData)) {
          cntRawData = cntRawData + 1
          //println(peekedVal.toString)
          //println(cntRawData.toString)
        }
        /*if (cntRawData == numberOfLoops) {
          expect(dut.out.bits.last, 1)
        }*/
      }
      step(1)
    }
  }
  step(150)
  cntValid = 0
  cntRawData = 0
  /*if (cntValid == (numberOfLoops*params.fft2ControlParams.numRxs)) {
    println("All output data are ok")
  }
  else {
    println("There are some mismatches in the output data")
    println(cntValid.toString)
  }*/
  step(20)
  //////////////////////// test new frame with same data ////////////////////////////////////
  //////////////////////// reset counters ///////////////////////////////////////////////////
  /*cntIn = 0
  cntValid = 0

  while (cntIn < numberOfLoops) {
    inValid = Random.nextInt(2)
    dut.ins.zipWithIndex.map { case (in, idx) =>
      poke(in.valid, inValid)
      if (peek(in.ready) == BigInt(1) && peek(in.valid) == BigInt(1)) {
        poke(in.bits.data, inData(cntIn + idx*numberOfLoops).toInt)
        if (cntIn == (numberOfLoops - 1)) {
          poke(in.bits.last, BigInt(1))
        }
      }
    }
    val readyIns = dut.ins.map { case in => if (peek(in.ready) == BigInt(1)) true else false }
    val readyAND = readyIns.reduce(_ && _)
    step(1)
    if (readyAND && inValid == 1) {
      cntIn = cntIn + 1
    }
  }

  dut.ins.foreach { in =>
    poke(in.valid, BigInt(0))
  }
  dut.ins.foreach { in =>
    poke(in.bits.last, BigInt(0))
  }
  // Random.setSeed(11110L)!
  step (20)
  if (params.fft2ControlParams.outputNodes == 1) {
     while (cntValid < totalOutData) {
      outReady = Random.nextInt(2)
      poke(dut.outs(0).ready, outReady)
      if (peek(dut.outs(0).ready) == BigInt(1) && peek(dut.outs(0).valid) == BigInt(1)) {
        peekedVal = peek(dut.outs(0).bits.data)
        assert(peekedVal.toInt == expected(cntValid))
        if (peekedVal.toInt == expected(cntValid)) {
          //println(cntValid.toString)
          cntValid = cntValid + 1
        }
        if (expected.length == 1) {
          expect(dut.outs(0).bits.last, 1)
        }
      }
      step(1)
    }
  }*/
}
class AXI4StreamFFT2RDWithTestStructuresSpec extends FlatSpec with Matchers {

  val numOfIterations = 4
  val rangeFFTSize = 256
  val dopplerFFTSize = 32
  val numTxs = 3
  val numRxs = 4

  implicit val p: Parameters = Parameters.empty

  for (i <-0 until numOfIterations) {
    for (dir <- Seq(false)) {//Seq(true, false)) {//, false)) {
      var readDir = if (dir) 1 else 0
      val inFileNameReal : String = f"./generators/dsp-blocks/fft2/python/gen_data_dir/realDataIn$readDir.txt"  // fft2rd
      val inFileNameImag : String = f"./generators/dsp-blocks/fft2/python/gen_data_dir/imagDataIn$readDir.txt"  // fft2rd
      val outFileNameReal: String = f"./generators/dsp-blocks/fft2/python/gen_data_dir/realDataOut$readDir.txt" // fft2rd
      val outFileNameImag: String = f"./generators/dsp-blocks/fft2/python/gen_data_dir/imagDataOut$readDir.txt" // fft2rd

      val paramsFFT2RDTS = FFT2RDTSParams(
        paramsFFT2RD = FFT2RDParams (
          rangeFFTParams = FFTParams.fixed(
            dataWidth = 12,
            twiddleWidth = 16,
            numPoints = rangeFFTSize,
            useBitReverse  = true,
            runTime = true,
            numAddPipes = 1,
            numMulPipes = 1,
            use4Muls = true,
            sdfRadix = "2",
            trimType = Convergent,
            expandLogic = Array.fill(log2Up(rangeFFTSize))(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
            keepMSBorLSB = Array.fill(log2Up(rangeFFTSize))(true),
            minSRAMdepth = 64,
            binPoint = 10
          ),
          dopplerFFTParams = FFTParams.fixed(
            dataWidth = 16,
            twiddleWidth = 16,
            numPoints = dopplerFFTSize,
            useBitReverse  = true,
            runTime = true,
            numAddPipes = 1,
            numMulPipes = 1,
            use4Muls = true,
            sdfRadix = "2",
            trimType = Convergent,
            expandLogic = Array.fill(log2Up(dopplerFFTSize))(0),
            keepMSBorLSB = Array.fill(log2Up(dopplerFFTSize))(true),
            minSRAMdepth = 8,
            binPoint = 10
          ),
          fft2ControlParams  = FFT2RDControlParams(rangeFFTSize = rangeFFTSize,
                                                dopplerFFTSize = dopplerFFTSize,
                                                numTxs = numTxs,
                                                numRxs = numRxs,
                                                outputNodes = 1,
                                                pingPong = false,
                                                readXYZorXZY = Some(dir)),
          // zeropadder parameters
          zeroPadderRangeParams = Some(ZeroPadderParams(
                                      proto = FixedPoint(16.W, 10.BP),
                                      packetSizeStart = 256,
                                      packetSizeEnd  = 256,       // range FFT size
                                      numberOfPackets = 32*3,     // dopplerFFTSize * numTxs
                                      useQueue = false,
                                      isDataComplex = true
                                    )
                                  ),
          zeroPadderDopplerParams = Some(ZeroPadderParams(
                                      proto = FixedPoint(16.W, 10.BP),
                                      packetSizeStart = 32,
                                      packetSizeEnd  = 32,
                                      numberOfPackets = 256*3*4,  // this highly depends on number of outputNodes!, if it is equal to 1 then then it is rangeFFTSize * numTxs * numRxs
                                      useQueue = false,
                                      isDataComplex = true
                                    )
                                  ),
          fft2ControlAddress = AddressSet(0x00000, 0xFFF),
          rangeFFTAddress    = AddressSet(0x01000, 0xFFF),
          dopplerFFTAddress  = AddressSet(0x02000, 0xFFF),
          zeroPadderRangeAddress  = Some(AddressSet(0x03000, 0xFFF)),
          zeroPadderDopplerAddress = Some(AddressSet(0x04000, 0xFFF))
        ),
        totalData = 256*32*3,
        beatBytes = 4,
        rawDataMuxAddress  = AddressSet(0x5000, 0xFF),
        storeRawMemAddress = AddressSet(0x6000, 0xFF)
      )
      val beatBytes = 4
      val testModule = LazyModule(new AXI4StreamFFT2RDWithTestStructuresBlock(paramsFFT2RDTS, beatBytes) with AXI4FFT2RDWithTestStructuresStandaloneBlock)
      it should f"test Range-Doppler 2D-FFT module with test structures, results are compared with Python model of 2D-FFT design - iteration $i, with read direction equal to $readDir," in {
        chisel3.iotesters.Driver.execute(Array("-fiwv",
              "--backend-name", "verilator"
              //"--tr-write-vcd",
              ), () => testModule.module) {
          c => new AXI4StreamFFT2RDWithTestStructuresBlockTester(dut = testModule,
                                              beatBytes = 4,
                                              params = paramsFFT2RDTS,
                                              inFileNameReal = inFileNameReal,
                                              inFileNameImag = inFileNameImag,
                                              outFileNameReal = outFileNameReal,
                                              outFileNameImag = outFileNameImag,
                                              silentFail  = false)
        } should be (true)
      }
    }
  }
}

