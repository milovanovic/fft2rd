package fft2rd

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

import fft._
import windowing._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import zeropadder._

trait AXI4FFT2RDStandaloneBlock extends AXI4StreamFFT2RDBlock[FixedPoint] {
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
    out := AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) := dopplerFFT.streamNode
    InModuleBody { out.makeIO() }
  }
  val nIn = params.fft2ControlParams.numRxs
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    if (params.zeroPadderRangeParams != None) {
      zeroPadderRange.get.streamNode :=  BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n    = beatBytes))) := in
    }
    else {
      if (params.winRangeParams != None) {
        windowingRange.get.streamNode :=  BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n    = beatBytes))) := in
      }
      else {
        rangeFFT.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) := in
      }
    }

    InModuleBody { in.makeIO() }
  }
}

// KRENI DA IZBACUJES JEDAN PO JEDAN POCEVSI OD IZLAZA!!!

class AXI4StreamFFT2RDBlock [T <: Data : Real: BinaryRepresentation] (val params: FFT2RDParams[T], val beatBytes: Int) extends LazyModule()(Parameters.empty) {
  // number of chirps
  val fft2Control = LazyModule(new AXI4StreamFFT2RDControlBlock(params.fft2ControlParams, params.fft2ControlAddress, beatBytes = beatBytes))
  val rangeFFT    = LazyModule(new AXI4MultipleFFTsBlock(params.rangeFFTParams, params.rangeFFTAddress, _beatBytes = beatBytes, configInterface = false))
  val dopplerFFT  = LazyModule(new AXI4MultipleFFTsBlock(params.dopplerFFTParams, params.dopplerFFTAddress, _beatBytes = beatBytes, configInterface = false))
  val zeroPadderRange = if (params.zeroPadderRangeParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.zeroPadderRangeParams.get, params.zeroPadderRangeAddress.get))) else None
  val zeroPadderDoppler = if (params.zeroPadderDopplerParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.zeroPadderDopplerParams.get, params.zeroPadderDopplerAddress.get))) else None
  val windowingRange = if (params.winRangeParams != None) Some(LazyModule(new WindowingBlockMultipleInOuts(params.winRangeAddress.get, params.winRangeRAMAddress, params.winRangeParams.get, beatBytes))) else None
  val windowingDoppler = if (params.winDopplerParams != None) Some(LazyModule(new WindowingBlockMultipleInOuts(params.winDopplerAddress.get, params.winDopplerRAMAddress, params.winDopplerParams.get, beatBytes))) else None

  for (i <- 0 until params.fft2ControlParams.numRxs) {
    if (params.zeroPadderRangeParams != None) {
      if (params.winRangeParams != None) {
        windowingRange.get.streamNode := zeroPadderRange.get.streamNode
        rangeFFT.streamNode := windowingRange.get.streamNode
      }
      else {
        rangeFFT.streamNode := zeroPadderRange.get.streamNode
      }
    //  fft2Control.streamNode := rangeFFT.streamNode   // AXI4StreamBuffer() - maybe not necessary to have - to be checked
    }
    else {
      if (params.winRangeParams != None) {
        rangeFFT.streamNode := windowingRange.get.streamNode
      }
    }
    fft2Control.streamNode := rangeFFT.streamNode   // AXI4StreamBuffer() - maybe not necessary to have - to be checked
  }
  for (i <- 0 until params.fft2ControlParams.outputNodes) {
    if (params.zeroPadderDopplerParams != None) {
      if (params.winDopplerParams != None) {
        windowingDoppler.get.streamNode := zeroPadderDoppler.get.streamNode
        dopplerFFT.streamNode := windowingDoppler.get.streamNode
        zeroPadderDoppler.get.streamNode := fft2Control.streamNode
      }
      else {
        dopplerFFT.streamNode  := zeroPadderDoppler.get.streamNode
        zeroPadderDoppler.get.streamNode := fft2Control.streamNode
      }
    }
    else {
      if (params.winDopplerParams != None) {
        windowingDoppler.get.streamNode := fft2Control.streamNode
        dopplerFFT.streamNode := windowingDoppler.get.streamNode
      }
      else {
        dopplerFFT.streamNode := fft2Control.streamNode // AXI4StreamBuffer()
      }
    }
  }

  var blocks = Seq(fft2Control, rangeFFT, dopplerFFT) // instead of lazy val, var is used
  if (params.zeroPadderRangeParams != None) {
    blocks = blocks :+ zeroPadderRange.get
  }
  if (params.zeroPadderDopplerParams != None) {
    blocks = blocks :+ zeroPadderDoppler.get
  }
  /*if (params.winRangeParams != None) {
    blocks = blocks :+ windowingRange.get
  }
  if (params.winDopplerParams != None) {
    blocks = blocks :+ windowingDoppler.get
  }*/
  //print(blocks.length)

  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)

  for (b <- blocks) {
    b.mem.foreach { _ := AXI4Buffer() := bus.node }
  }
  if (params.winRangeParams != None) {
    windowingRange.get.windowing.mem.get := bus.node
  }
  if (params.winDopplerParams != None) {
    windowingDoppler.get.windowing.mem.get := bus.node
  }

  lazy val module = new LazyModuleImp(this) {}
}

object FFT2RDDspBlockAXI4 extends App
{
  val rangeFFTSize = 256
  val dopplerFFTSize = 32
  val numTxs = 4
  val numRxs = 8
  val outputNodes = 32

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
        minSRAMdepth = 8,
        binPoint = 10
      ),
      fft2ControlParams  = FFT2RDControlParams(rangeFFTSize = rangeFFTSize,
                                            dopplerFFTSize = dopplerFFTSize,
                                            numTxs = numTxs,
                                            numRxs = numRxs,
                                            outputNodes = outputNodes,
                                            pingPong = false,
                                            readXYZorXZY = Some(false)),
      // zeropadder parameters
      zeroPadderRangeParams = None, /*Some(ZeroPadderParams(
                                  proto = FixedPoint(16.W, 10.BP),
                                  packetSizeStart = 256,
                                  packetSizeEnd  = 256,       // range FFT size
                                  numberOfPackets = (dopplerFFTSize*numTxs),     // dopplerFFTSize * numTxs
                                  useQueue = false,
                                  isDataComplex = true
                                )
                              ),*/
      zeroPadderDopplerParams = None, /*Some(ZeroPadderParams(
                                  proto = FixedPoint(16.W, 10.BP),
                                  packetSizeStart = 32,
                                  packetSizeEnd  = 32,
                                  numberOfPackets = (rangeFFTSize*numTxs*numRxs)/outputNodes,  // this highly depends on number of outputNodes!, if it is equal to 1 then then it is rangeFFTSize * numTxs * numRxs
                                  useQueue = false,
                                  isDataComplex = true
                                )
                              ),*/
      winRangeParams = Some(WindowingParams.fixed(
                        dataWidth = 16,
                        binPoint = 14,
                        numMulPipes = 1,
                        numPoints = rangeFFTSize,
                        trimType = Convergent,
                        dirName = "test_run_dir",
                        memoryFile = "./test_run_dir/blacman.txt",
                        constWindow = false,
                        windowFunc = WindowFunctionTypes.Blackman(dataWidth_tmp = 16)
                      )
                    ),
      winDopplerParams = Some(WindowingParams.fixed(
                        dataWidth = 16,
                        binPoint = 14,
                        numMulPipes = 1,
                        numPoints = dopplerFFTSize,
                        trimType = Convergent,
                        dirName = "test_run_dir",
                        memoryFile = "./test_run_dir/blacman.txt",
                        constWindow = false,
                        windowFunc = WindowFunctionTypes.Blackman(dataWidth_tmp = 16)
                      )
                    ),
      zeroPadderRangeAddress  = None,  //Some(AddressSet(0x03000, 0xFFF)),
      zeroPadderDopplerAddress = None, //Some(AddressSet(0x04000, 0xFFF)),
      winDopplerAddress = Some(AddressSet(0x05000, 0xFFF)),
      winRangeAddress = Some(AddressSet(0x06000, 0xFFF)),
      winDopplerRAMAddress = Some(AddressSet(0x10000, 0xFFFF)), // constWindow needs to be set to true if it is None
      winRangeRAMAddress = Some(AddressSet(0x20000, 0xFFFF)),
      fft2ControlAddress = AddressSet(0x00000, 0xFFF),
      rangeFFTAddress    = AddressSet(0x01000, 0xFFF),
      dopplerFFTAddress  = AddressSet(0x02000, 0xFFF)
    )
  implicit val p: Parameters = Parameters.empty

  val fft2Module = LazyModule(new AXI4StreamFFT2RDBlock(paramsFFT2RD, beatBytes = 4) with AXI4FFT2RDStandaloneBlock)
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => fft2Module.module)))
}
