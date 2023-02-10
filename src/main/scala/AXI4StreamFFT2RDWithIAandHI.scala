package fft2rd

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import dsptools.numbers._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

import fft._
import utils._
import zeropadder._

trait AXI4FFT2RDWithIAandHIStandaloneBlock extends AXI4StreamFFT2RDWithIAandHI[FixedPoint] {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)

  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
    BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
    ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val nOut = params.fft2ControlParams.outputNodes
  val outs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until nOut) yield {
    implicit val valName = ValName(s"outIO_$o")
    val out = BundleBridgeSink[AXI4StreamBundle]()
    out := AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) := AXI4StreamBuffer() := headerInserter.streamNode
    InModuleBody { out.makeIO() }
  }
  val nIn = 1
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    inputAdapter.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) := in
    InModuleBody { in.makeIO() }
  }
}

class AXI4StreamFFT2RDWithIAandHI [T <: Data : Real: BinaryRepresentation] (val params: FFT2RDParams[T], mode1024: Boolean = true, hIAddress: AddressSet, val beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val inputAdapter = LazyModule(new AXI4StreamAdapter1to4(beatBytes = beatBytes))
  val fft2Control = LazyModule(new AXI4StreamFFT2RDControlBlock(params.fft2ControlParams, params.fft2ControlAddress, beatBytes = beatBytes))
  val rangeFFT    = LazyModule(new AXI4MultipleFFTsBlock(params.rangeFFTParams, params.rangeFFTAddress, _beatBytes = beatBytes, configInterface = false))
  val dopplerFFT  = LazyModule(new AXI4MultipleFFTsBlock(params.dopplerFFTParams, params.dopplerFFTAddress, _beatBytes = beatBytes, configInterface = false))
  val zeroPadderRange = if (params.zeroPadderRangeParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.zeroPadderRangeParams.get, params.zeroPadderRangeAddress.get))) else None
  val zeroPadderDoppler = if (params.zeroPadderDopplerParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.zeroPadderDopplerParams.get, params.zeroPadderDopplerAddress.get))) else None
  val headerInserter = LazyModule(new AXI4HeaderInserterBlock(mode1024, hIAddress, beatBytes))

  for (i <- 0 until params.fft2ControlParams.numRxs) {
    if (params.zeroPadderRangeParams != None) {
      zeroPadderRange.get.streamNode := AXI4StreamBuffer() := inputAdapter.streamNode
      rangeFFT.streamNode := AXI4StreamBuffer() := zeroPadderRange.get.streamNode
      fft2Control.streamNode := AXI4StreamBuffer() := rangeFFT.streamNode
    }
    else {
      rangeFFT.streamNode := inputAdapter.streamNode
      fft2Control.streamNode := rangeFFT.streamNode
    }
  }
  for (i <- 0 until params.fft2ControlParams.outputNodes) {
    if (params.zeroPadderDopplerParams != None) {
      dopplerFFT.streamNode  := AXI4StreamBuffer() := zeroPadderDoppler.get.streamNode
      zeroPadderDoppler.get.streamNode := AXI4StreamBuffer() := fft2Control.streamNode
      headerInserter.streamNode :=  AXI4StreamBuffer() := dopplerFFT.streamNode
    }
    else {
      dopplerFFT.streamNode := AXI4StreamBuffer() := fft2Control.streamNode
      headerInserter.streamNode := AXI4StreamBuffer() := dopplerFFT.streamNode
    }
  }

  var blocks = Seq(fft2Control, rangeFFT, dopplerFFT, headerInserter) // instead of lazy val, var is used
  if (params.zeroPadderRangeParams != None) {
    blocks = blocks :+ zeroPadderRange.get
  }
  if (params.zeroPadderDopplerParams != None) {
    blocks = blocks :+ zeroPadderDoppler.get
  }
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)

  for (b <- blocks) {
    b.mem.foreach { _ := AXI4Buffer() := bus.node }
  }

  lazy val module = new LazyModuleImp(this) {}
}

object FFT2RDWithIAandHIDspBlockAXI4 extends App
{
  val rangeFFTSize = 256
  val dopplerFFTSize = 32
  val numTxs = 3
  val numRxs = 4

  val paramsFFT2RD: FFT2RDParams[FixedPoint] = FFT2RDParams (
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
        minSRAMdepth = 64,
        binPoint = 10
      ),
      fft2ControlParams  = FFT2RDControlParams(rangeFFTSize = rangeFFTSize,
                                            dopplerFFTSize = dopplerFFTSize,
                                            numTxs = numTxs,
                                            numRxs = numRxs,
                                            outputNodes = 1,
                                            pingPong = false,
                                            readXYZorXZY = Some(false)),
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
      zeroPadderRangeAddress  = Some(AddressSet(0x80003000L, 0xFFF)),
      zeroPadderDopplerAddress = Some(AddressSet(0x80004000L, 0xFFF)),
      fft2ControlAddress = AddressSet(0x80000000L, 0xFFF),
      rangeFFTAddress    = AddressSet(0x80001000L, 0xFFF),
      dopplerFFTAddress  = AddressSet(0x80002000L, 0xFFF)
    )
  val hiAddress = AddressSet(0x80005000L, 0xFFF)

  val mode1024 = false
  implicit val p: Parameters = Parameters.empty
  val fft2Module = LazyModule(new AXI4StreamFFT2RDWithIAandHI(paramsFFT2RD, mode1024, hiAddress, beatBytes = 4) with AXI4FFT2RDWithIAandHIStandaloneBlock)
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => fft2Module.module)))
}
