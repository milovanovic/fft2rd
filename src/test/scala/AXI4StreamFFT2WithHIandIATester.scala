package fft2rd

import chisel3.util._
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import fixedpoint._
import chiseltest.iotesters.PeekPokeTester
import fft._
import zeropadder._
import windowing._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters

import scala.util.Random
import scala.io.Source
import dsptools.numbers._

// Works ok when zeropadder is on and when it is off
class AXI4StreamFFT2RDFT2WithIAandHIBlockTester(
  dut:                AXI4StreamFFT2RDWithIAandHI[FixedPoint] with AXI4FFT2RDWithIAandHIStandaloneBlock,
  params:             FFT2RDParams[FixedPoint],
  inFileNameComplex:  String,
  outFileNameComplex: String,
  beatBytes:          Int = 4,
  silentFail:         Boolean = false)
    extends PeekPokeTester(dut.module)
    with AXI4MasterModel {

  def readFile(filename: String): Array[BigInt] = { // data are hex data
    val bufferedSource = Source.fromFile(filename)
    val contents = Source.fromFile(filename).getLines.toArray.map { br => BigInt(br, 16) }
    bufferedSource.close
    contents
  }

  override def memAXI: AXI4Bundle = dut.ioMem.get
  val mod = dut.module

  val inData = readFile(inFileNameComplex)
  val expected = readFile(outFileNameComplex)
  val numberOfLoops =
    params.fft2ControlParams.rangeFFTSize * params.fft2ControlParams.dopplerFFTSize * params.fft2ControlParams.numTxs
  val radarDataCubeSize =
    numberOfLoops * params.fft2ControlParams.numRxs // this is all for number of outputNodes equal to 1
  val numOfOutData = expected.length
  poke(dut.outs(0).ready, 1)

  if (params.winRangeParams != None) {
    //  memWriteWord(params.winRangeAddress.get.base + beatBytes, 1) // enable windowing!
  }

  var inValid = 0
  var cntIn = 0

  while (cntIn < radarDataCubeSize) {
    inValid = Random.nextInt(2)
    poke(dut.ins(0).valid, inValid)
    if (peek(dut.ins(0).ready) == BigInt(1) && peek(dut.ins(0).valid) == BigInt(1)) {
      poke(dut.ins(0).bits.data, inData(cntIn).toInt)
      cntIn = cntIn + 1
      /*if (cntIn == (numberOfLoops - 1)) {
        poke(dut.ins(0).bits.last, BigInt(1))
      }*/ // last in signal generation!
    }
    step(1)
  }
  poke(dut.ins(0).valid, BigInt(0))
  poke(dut.ins(0).bits.last, BigInt(0))

  //Random.setSeed(11110L)
  cntIn = 0

  step(20)
  var cntValid = 0
  var peekedVal: BigInt = 0
  var outReady = 0

  if (params.fft2ControlParams.outputNodes == 1) {
    while (cntValid < numOfOutData) {
      outReady = Random.nextInt(2)
      poke(dut.outs(0).ready, outReady)
      if (peek(dut.outs(0).ready) == BigInt(1) && peek(dut.outs(0).valid) == BigInt(1)) {
        peekedVal = peek(dut.outs(0).bits.data)
        assert(peekedVal == expected(cntValid), f"Assertion on $cntValid sample")
        cntValid = cntValid + 1
        if ((cntValid % 193) == 0) {
          expect(dut.outs(0).bits.last, 1)
        }
      }
      step(1)
    }
  }

  step(20)
  //////////////////////// test new frame with same data ////////////////////////////////////
  //////////////////////// reset counters ///////////////////////////////////////////////////
//   cntIn = 0
//   cntValid = 0
//
//   while (cntIn < radarDataCubeSize) {
//     inValid = Random.nextInt(2)
//     poke(dut.ins(0).valid, inValid)
//     if (peek(dut.ins(0).ready) == BigInt(1) && peek(dut.ins(0).valid) == BigInt(1)) {
//       poke(dut.ins(0).bits.data, inData(cntIn).toInt)
//       cntIn = cntIn + 1
//       /*if (cntIn == (numberOfLoops - 1)) {
//         poke(dut.ins(0).bits.last, BigInt(1))
//       }*/ // last in signal generation!
//     }
//     step(1)
//   }
//   poke(dut.ins(0).valid, BigInt(0))
//   poke(dut.ins(0).bits.last, BigInt(0))
//   Random.setSeed(11110L)
//
//   if (params.fft2ControlParams.outputNodes == 1) {
//     while (cntValid < numOfOutData) {
//       outReady = Random.nextInt(2)
//       poke(dut.outs(0).ready, outReady)
//       if (peek(dut.outs(0).ready) == BigInt(1) && peek(dut.outs(0).valid) == BigInt(1)) {
//         peekedVal = peek(dut.outs(0).bits.data)
//         assert(peekedVal == expected(cntValid), f"Assertion on $cntValid sample")
//         cntValid = cntValid + 1
//         if ((cntValid % 193) == 0) {
//           expect(dut.outs(0).bits.last, 1)
//         }
//       }
//       step(1)
//     }
//   }
  step(20)
}
class AXI4StreamFFT2RDWithIAandHIBlockSpec extends AnyFlatSpec with ChiselScalatestTester {

  val numOfIterations = 1
  val rangeFFTSize = 256
  val dopplerFFTSize = 32
  val numTxs = 3
  val numRxs = 4

  implicit val p: Parameters = Parameters.empty

  for (i <- 0 until numOfIterations) {
    for (dir <- Seq(false)) { //, false)) {
      var readDir = if (dir) 1 else 0
      val inFileNameComplex: String =
        //f"./generators/dsp-blocks/fft2rd/python/gen_data_dir/complexDataInHexWithInputAdapter$readDir.txt"
        f"./python/gen_data_dir/complexDataInHexWithInputAdapter$readDir.txt"
      val outFileNameComplex: String =
        //f"./generators/dsp-blocks/fft2rd/python/gen_data_dir/complexDataOutHexWithHeaderInserter$readDir.txt"
        f"./python/gen_data_dir/complexDataOutHexWithHeaderInserter$readDir.txt"

      val paramsFFT2RD: FFT2RDParams[FixedPoint] = FFT2RDParams(
        rangeFFTParams = FFTParams.fixed(
          dataWidth = 12,
          twiddleWidth = 16,
          numPoints = rangeFFTSize,
          useBitReverse = true,
          runTime = true,
          numAddPipes = 1,
          numMulPipes = 1,
          use4Muls = true,
          sdfRadix = "2",
          trimType = Convergent,
          expandLogic = Array.fill(log2Up(rangeFFTSize))(1).zipWithIndex.map {
            case (e, ind) => if (ind < 4) 1 else 0
          }, // expand first four stages, other do not grow
          keepMSBorLSB = Array.fill(log2Up(rangeFFTSize))(true),
          minSRAMdepth = 64,
          binPoint = 10
        ),
        dopplerFFTParams = FFTParams.fixed(
          dataWidth = 16,
          twiddleWidth = 16,
          numPoints = dopplerFFTSize,
          useBitReverse = true,
          runTime = true,
          numAddPipes = 1,
          numMulPipes = 1,
          use4Muls = true,
          sdfRadix = "2",
          trimType = Convergent,
          expandLogic = Array.fill(log2Up(dopplerFFTSize))(0),
          keepMSBorLSB = Array.fill(log2Up(dopplerFFTSize))(true),
          minSRAMdepth = 64,
          binPoint = 10
        ),
        fft2ControlParams = FFT2RDControlParams(
          rangeFFTSize = rangeFFTSize,
          dopplerFFTSize = dopplerFFTSize,
          numTxs = numTxs,
          numRxs = numRxs,
          outputNodes = 1,
          pingPong = false,
          readXYZorXZY = Some(dir)
        ),
        // zeropadder parameters
        zeroPadderRangeParams = Some(
          ZeroPadderParams(
            proto = FixedPoint(16.W, 10.BP),
            packetSizeStart = 256,
            packetSizeEnd = 256, // range FFT size
            numberOfPackets = 32 * 3, // dopplerFFTSize * numTxs
            useQueue = false,
            isDataComplex = true
          )
        ),
        zeroPadderDopplerParams = Some(
          ZeroPadderParams(
            proto = FixedPoint(16.W, 10.BP),
            packetSizeStart = 32,
            packetSizeEnd = 32,
            numberOfPackets =
              256 * 3 * 4, // this highly depends on number of outputNodes!, if it is equal to 1 then then it is rangeFFTSize * numTxs * numRxs
            useQueue = false,
            isDataComplex = true
          )
        ),
        winRangeParams = Some(
          WindowingParams.fixed(
            dataWidth = 12,
            binPoint = 10,
            numMulPipes = 1,
            numPoints = rangeFFTSize,
            trimType = Convergent,
            dirName = "test_run_dir",
            memoryFile = "./test_run_dir/blacman.txt",
            constWindow = true,
            windowFunc = WindowFunctionTypes.Blackman(dataWidth_tmp = 12)
          )
        ),
        winDopplerParams = Some(
          WindowingParams.fixed(
            dataWidth = 16,
            binPoint = 10,
            numMulPipes = 1,
            numPoints = dopplerFFTSize,
            trimType = Convergent,
            dirName = "test_run_dir",
            memoryFile = "./test_run_dir/blacman.txt",
            constWindow = true,
            windowFunc = WindowFunctionTypes.Hamming(dataWidth_tmp = 12)
          )
        ),
        winDopplerAddress = Some(AddressSet(0x06000L, 0xfff)),
        winRangeAddress = Some(AddressSet(0x07000L, 0xfff)),
        winDopplerRAMAddress = None, //Some(AddressSet(0x10000, 0xFFFF)),
        winRangeRAMAddress = None, //Some(AddressSet(0x20000, 0xFFFF)),
        zeroPadderRangeAddress = Some(AddressSet(0x03000, 0xfff)),
        zeroPadderDopplerAddress = Some(AddressSet(0x04000, 0xfff)),
        fft2ControlAddress = AddressSet(0x00000, 0xfff),
        rangeFFTAddress = AddressSet(0x01000, 0xfff),
        dopplerFFTAddress = AddressSet(0x02000, 0xfff)
      )
      val beatBytes = 4
      val testModule = LazyModule(
        new AXI4StreamFFT2RDWithIAandHI(paramsFFT2RD, false, AddressSet(0x05000, 0xfff), beatBytes)
          with AXI4FFT2RDWithIAandHIStandaloneBlock
      )
      it should f"test Range-Doppler 2D-FFT module, results are compared with Python model of 2D-FFT design - iteration $i, with read direction equal to $readDir," in {
        test(testModule.module)
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
          .runPeekPoke(_ =>
            new AXI4StreamFFT2RDFT2WithIAandHIBlockTester(
              dut = testModule,
              beatBytes = 4,
              params = paramsFFT2RD,
              inFileNameComplex = inFileNameComplex,
              outFileNameComplex = outFileNameComplex,
              silentFail = false
            )
          )

        /*chisel3.iotesters.Driver.execute(Array("-fiwv", "--backend-name", "verilator"), () => testModule.module) { c =>
          new AXI4StreamFFT2RDFT2WithIAandHIBlockTester(
            dut = testModule,
            beatBytes = 4,
            params = paramsFFT2RD,
            inFileNameComplex = inFileNameComplex,
            outFileNameComplex = outFileNameComplex,
            silentFail = false
          )
        } should be(true)*/
      }
    }
  }
}
