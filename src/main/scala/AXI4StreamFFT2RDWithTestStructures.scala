package fft2rd

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import fft._
import zeropadder._
import utils._

case class FFT2RDTSParams (
  paramsFFT2RD        : FFT2RDParams[FixedPoint],
  storeRawMemAddress: AddressSet,
  rawDataMuxAddress : AddressSet,
  totalData         : Int,
  beatBytes         : Int
)

//Note: Module AXI4StreamFFT2RDWithTestStructuresBlock can be used only when number of output nodes is equal to 1
trait AXI4FFT2RDWithTestStructuresStandaloneBlock extends AXI4StreamFFT2RDWithTestStructuresBlock {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)

  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
    BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
    ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outMux.masterNode
  val out = InModuleBody { ioOutNode.makeIO() }

  val nIn = params.paramsFFT2RD.fft2ControlParams.numRxs
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    if (params.paramsFFT2RD.zeroPadderRangeParams != None) {
      zeroPadderRange.get.streamNode :=  BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n    = beatBytes))) := in
    }
    else {
      rawDataSplit(i).streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) := in
    }

    InModuleBody { in.makeIO() }
  }
}

class AXI4StreamFFT2RDWithTestStructuresBlock(val params: FFT2RDTSParams, val beatBytes: Int) extends LazyModule()(Parameters.empty) {
  // number of chirps
  val fft2Control = LazyModule(new AXI4StreamFFT2RDControlBlock(params.paramsFFT2RD.fft2ControlParams, params.paramsFFT2RD.fft2ControlAddress, beatBytes = beatBytes))
  val rangeFFT    = LazyModule(new AXI4MultipleFFTsBlock(params.paramsFFT2RD.rangeFFTParams, params.paramsFFT2RD.rangeFFTAddress, _beatBytes = beatBytes, configInterface = false))
  val dopplerFFT  = LazyModule(new AXI4MultipleFFTsBlock(params.paramsFFT2RD.dopplerFFTParams, params.paramsFFT2RD.dopplerFFTAddress, _beatBytes = beatBytes, configInterface = false))
  val zeroPadderRange = if (params.paramsFFT2RD.zeroPadderRangeParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.paramsFFT2RD.zeroPadderRangeParams.get, params.paramsFFT2RD.zeroPadderRangeAddress.get))) else None
  val zeroPadderDoppler = if (params.paramsFFT2RD.zeroPadderDopplerParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.paramsFFT2RD.zeroPadderDopplerParams.get, params.paramsFFT2RD.zeroPadderDopplerAddress.get))) else None

  val rawDataSplit = Seq.fill(params.paramsFFT2RD.fft2ControlParams.numRxs)(LazyModule(new SplitterAND(beatBytes = params.beatBytes)))                // defined in dsputils
  val rawDataMux =  LazyModule(new AXI4StreamCustomMux(address = params.rawDataMuxAddress,  beatBytes = params.beatBytes)) // defined in dsputils
  // totalData needs to be equal to numTxs*rangeFFTsize*dopplerFFTSize
  val storeRawMem = LazyModule(new AXI4StreamDataStoringModule(address = params.storeRawMemAddress, beatBytes = params.beatBytes, totalData = params.totalData, triggerIn = true)) // defined in dsputils
  val dopplerFFTWrapper = LazyModule(new AXI4StreamMultipleIdentityWrapper(4, params.paramsFFT2RD.fft2ControlParams.outputNodes)) // Todo: Move AXI4StreamMultipleIdentityWrapper to dsputils
  val outMux = LazyModule(new AXI4Stream2InputMux(beatBytes = beatBytes)) //LazyModule(new AXI4StreamSimpleMux(numStreams = 1, beatBytes = beatBytes))

  for (i <- 0 until params.paramsFFT2RD.fft2ControlParams.numRxs) {
    if (params.paramsFFT2RD.zeroPadderRangeParams != None) {
      rawDataSplit(i).streamNode := AXI4StreamBuffer() := zeroPadderRange.get.streamNode
      rangeFFT.streamNode := AXI4StreamBuffer() := rawDataSplit(i).streamNode
      rawDataMux.streamNode := AXI4StreamBuffer() := rawDataSplit(i).streamNode
     // rangeFFT.streamNode := zeroPadderRange.get.streamNode
      fft2Control.streamNode := AXI4StreamBuffer() := rangeFFT.streamNode   // AXI4StreamBuffer() - maybe not necessary to have - to be checked
    }
    else {
      rangeFFT.streamNode := AXI4StreamBuffer() := rawDataSplit(i).streamNode
      rawDataMux.streamNode := AXI4StreamBuffer() := rawDataSplit(i).streamNode
      fft2Control.streamNode := AXI4StreamBuffer() := rangeFFT.streamNode   // AXI4StreamBuffer() - maybe not necessary to have - to be checked
    }
  }
  for (i <- 0 until params.paramsFFT2RD.fft2ControlParams.outputNodes) {
    if (params.paramsFFT2RD.zeroPadderDopplerParams != None) {
      dopplerFFT.streamNode  := AXI4StreamBuffer() := zeroPadderDoppler.get.streamNode
      dopplerFFTWrapper.streamNode(i) :=  AXI4StreamBuffer() := dopplerFFT.streamNode
      zeroPadderDoppler.get.streamNode := AXI4StreamBuffer() := fft2Control.streamNode
    }
    else {
      dopplerFFT.streamNode := AXI4StreamBuffer() := fft2Control.streamNode // AXI4StreamBuffer()
      dopplerFFTWrapper.streamNode(i) := AXI4StreamBuffer() := dopplerFFT.streamNode
    }
  }
  var blocks = Seq(fft2Control, rangeFFT, dopplerFFT, rawDataMux, storeRawMem) // instead of lazy val, var is used
  if (params.paramsFFT2RD.zeroPadderRangeParams != None) {
    blocks = blocks :+ zeroPadderRange.get
  }
  if (params.paramsFFT2RD.zeroPadderDopplerParams != None) {
    blocks = blocks :+ zeroPadderDoppler.get
  }
  //print(blocks.length)
  storeRawMem.streamNode  := rawDataMux.streamNode
  outMux.slaveNode1 := dopplerFFTWrapper.streamNode(0)
  outMux.slaveNode2 := storeRawMem.streamNode

  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)

  for (b <- blocks) {
    b.mem.foreach { _ := AXI4Buffer() := bus.node }
  }

  lazy val module = new LazyModuleImp(this) {
    outMux.module.sel := storeRawMem.module.triggerOut.get
    storeRawMem.module.trigger.get := dopplerFFTWrapper.module.io(0).last_out
  }
}

object FFT2RDWithTestStructuresDspBlockAXI4 extends App
{
  val paramsFFT2RDTS = FFT2RDTSParams(
    paramsFFT2RD = FFT2RDParams (
      rangeFFTParams = FFTParams.fixed(
        dataWidth = 12,
        twiddleWidth = 16,
        numPoints = 256,
        useBitReverse  = true,
        runTime = true,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        expandLogic = Array.fill(log2Up(256))(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 },
        trimType = Convergent,
        sdfRadix = "2",
        keepMSBorLSB = Array.fill(log2Up(256))(true),
        minSRAMdepth = 64,
        binPoint = 10
      ),
      dopplerFFTParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = 32,
        useBitReverse  = true,
        runTime = true,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        trimType = Convergent,
        sdfRadix = "2",
        expandLogic = Array.fill(log2Up(32))(0),
        keepMSBorLSB = Array.fill(log2Up(32))(true),
        minSRAMdepth = 32,
        binPoint = 10
      ),
      zeroPadderRangeParams = Some(ZeroPadderParams(
                                      proto = FixedPoint(16.W, 10.BP),
                                      packetSizeStart = 256,
                                      packetSizeEnd  = 256, // range FFT size
                                      numberOfPackets = 32*3,
                                      useQueue = false,
                                      isDataComplex = true
                                    )
                                  ),
      zeroPadderDopplerParams = Some(ZeroPadderParams(
                                      proto = FixedPoint(16.W, 10.BP),
                                      packetSizeStart = 32,
                                      packetSizeEnd  = 32,
                                      numberOfPackets = 256*3*4,  // this highly depends on number of outputNodes, if it is equal to 1 then then it is rangeFFTSize * numTxs * numRxs
                                      useQueue = false,
                                      isDataComplex = true
                                    )
                                  ),
      fft2ControlParams  = FFT2RDControlParams(rangeFFTSize = 256, dopplerFFTSize = 32, numTxs = 3, numRxs = 4, outputNodes = 1, pingPong = false),
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
  implicit val p: Parameters = Parameters.empty

  val fft2Module = LazyModule(new AXI4StreamFFT2RDWithTestStructuresBlock(paramsFFT2RDTS, beatBytes = 4) with AXI4FFT2RDWithTestStructuresStandaloneBlock)
  chisel3.Driver.execute(args, ()=> fft2Module.module)
}

// package fft2
//
// import chisel3._
// import chisel3.util._
// import chisel3.experimental._
//
// import dsptools.numbers._
// import freechips.rocketchip.amba.axi4._
// import freechips.rocketchip.amba.axi4stream._
// import freechips.rocketchip.config._
// import freechips.rocketchip.diplomacy._
// import fft._
// import zeropadder._
// import dsputils._
//
// case class FFT2RDTSParams (
//   paramsFFT2RD        : FFT2RDParams[FixedPoint],
//   storeRawMemAddress: AddressSet,
//   rawDataMuxAddress : AddressSet,
//   totalData         : Int,
//   beatBytes         : Int
// )
//
// //Note: Module AXI4StreamFFT2RDWithTestStructuresBlock can be used only when number of output nodes is equal to 1
// trait AXI4FFT2RDWithTestStructuresStandaloneBlock extends AXI4StreamFFT2RDWithTestStructuresBlock {
//   def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
//
//   val ioMem = mem.map { m => {
//     val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
//
//     m :=
//     BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
//     ioMemNode
//
//     val ioMem = InModuleBody { ioMemNode.makeIO() }
//     ioMem
//   }}
//
//   val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
//   ioOutNode :=
//     AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outMux.masterNode
//   val out = InModuleBody { ioOutNode.makeIO() }
//
//   val nIn = params.paramsFFT2RD.fft2ControlParams.numRxs
//   val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
//     implicit val valName = ValName(s"inIO_$i")
//     val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
//     if (params.paramsFFT2RD.zeroPadderRangeParams != None) {
//       zeroPadderRange.get.streamNode :=  BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n    = beatBytes))) := in
//     }
//     else {
//       rawDataSplit(i).streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) := in
//     }
//
//     InModuleBody { in.makeIO() }
//   }
// }
//
// class AXI4StreamFFT2RDWithTestStructuresBlock(val params: FFT2RDTSParams, val beatBytes: Int) extends LazyModule()(Parameters.empty) {
//   // number of chirps
//   val fft2Control = LazyModule(new AXI4StreamFFT2RDControlBlock(params.paramsFFT2RD.fft2ControlParams, params.paramsFFT2RD.fft2ControlAddress, beatBytes = beatBytes))
//   val rangeFFT    = LazyModule(new AXI4MultipleFFTsBlock(params.paramsFFT2RD.rangeFFTParams, params.paramsFFT2RD.rangeFFTAddress, _beatBytes = beatBytes, configInterface = false))
//   val dopplerFFT  = LazyModule(new AXI4MultipleFFTsBlock(params.paramsFFT2RD.dopplerFFTParams, params.paramsFFT2RD.dopplerFFTAddress, _beatBytes = beatBytes, configInterface = false))
//   val zeroPadderRange = if (params.paramsFFT2RD.zeroPadderRangeParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.paramsFFT2RD.zeroPadderRangeParams.get, params.paramsFFT2RD.zeroPadderRangeAddress.get))) else None
//   val zeroPadderDoppler = if (params.paramsFFT2RD.zeroPadderDopplerParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.paramsFFT2RD.zeroPadderDopplerParams.get, params.paramsFFT2RD.zeroPadderDopplerAddress.get))) else None
//
//   val rawDataSplit = Seq.fill(params.paramsFFT2RD.fft2ControlParams.numRxs)(LazyModule(new SplitterAND(beatBytes = params.beatBytes)))                // defined in dsputils
//   val rawDataMux =  LazyModule(new AXI4StreamCustomMux(address = params.rawDataMuxAddress,  beatBytes = params.beatBytes)) // defined in dsputils
//   // totalData needs to be equal to numTxs*rangeFFTsize*dopplerFFTSize
//   val storeRawMem = LazyModule(new AXI4StreamDataStoringModule(address = params.storeRawMemAddress, beatBytes = params.beatBytes, totalData = params.totalData, triggerIn = true)) // defined in dsputils
//   val dopplerFFTWrapper = LazyModule(new AXI4StreamMultipleIdentityWrapper(4, params.paramsFFT2RD.fft2ControlParams.outputNodes)) // Todo: Move AXI4StreamMultipleIdentityWrapper to dsputils
//   val outMux = LazyModule(new AXI4Stream2InputMux(beatBytes = beatBytes)) //LazyModule(new AXI4StreamSimpleMux(numStreams = 1, beatBytes = beatBytes))
//
//   for (i <- 0 until params.paramsFFT2RD.fft2ControlParams.numRxs) {
//     if (params.paramsFFT2RD.zeroPadderRangeParams != None) {
//       rawDataSplit(i).streamNode := zeroPadderRange.get.streamNode
//       rangeFFT.streamNode := rawDataSplit(i).streamNode
//       rawDataMux.streamNode := rawDataSplit(i).streamNode
//      // rangeFFT.streamNode := zeroPadderRange.get.streamNode
//       fft2Control.streamNode := rangeFFT.streamNode   // AXI4StreamBuffer() - maybe not necessary to have - to be checked
//     }
//     else {
//       rangeFFT.streamNode := rawDataSplit(i).streamNode
//       rawDataMux.streamNode := rawDataSplit(i).streamNode
//       fft2Control.streamNode := rangeFFT.streamNode   // AXI4StreamBuffer() - maybe not necessary to have - to be checked
//     }
//   }
//   for (i <- 0 until params.paramsFFT2RD.fft2ControlParams.outputNodes) {
//     if (params.paramsFFT2RD.zeroPadderDopplerParams != None) {
//       dopplerFFT.streamNode  := zeroPadderDoppler.get.streamNode
//       dopplerFFTWrapper.streamNode(i) := dopplerFFT.streamNode
//       zeroPadderDoppler.get.streamNode := fft2Control.streamNode
//     }
//     else {
//       dopplerFFT.streamNode := fft2Control.streamNode // AXI4StreamBuffer()
//       dopplerFFTWrapper.streamNode(i) := dopplerFFT.streamNode
//     }
//   }
//   var blocks = Seq(fft2Control, rangeFFT, dopplerFFT, rawDataMux, storeRawMem) // instead of lazy val, var is used
//   if (params.paramsFFT2RD.zeroPadderRangeParams != None) {
//     blocks = blocks :+ zeroPadderRange.get
//   }
//   if (params.paramsFFT2RD.zeroPadderDopplerParams != None) {
//     blocks = blocks :+ zeroPadderDoppler.get
//   }
//   //print(blocks.length)
//   storeRawMem.streamNode  := rawDataMux.streamNode
//   outMux.slaveNode1 := dopplerFFTWrapper.streamNode(0)
//   outMux.slaveNode2 := storeRawMem.streamNode
//
//   val bus = LazyModule(new AXI4Xbar)
//   val mem = Some(bus.node)
//
//   for (b <- blocks) {
//     b.mem.foreach { _ := AXI4Buffer() := bus.node }
//   }
//
//   lazy val module = new LazyModuleImp(this) {
//     outMux.module.sel := storeRawMem.module.triggerOut.get
//     storeRawMem.module.trigger.get := dopplerFFTWrapper.module.io(0).last_out
//   }
// }
//
// object FFT2RDWithTestStructuresDspBlockAXI4 extends App
// {
//   val paramsFFT2RDTS = FFT2RDTSParams(
//     paramsFFT2RD = FFT2RDParams (
//       rangeFFTParams = FFTParams.fixed(
//         dataWidth = 12,
//         twiddleWidth = 16,
//         numPoints = 256,
//         useBitReverse  = true,
//         runTime = true,
//         numAddPipes = 1,
//         numMulPipes = 1,
//         use4Muls = true,
//         expandLogic = Array.fill(log2Up(256))(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 },
//         trimType = Convergent,
//         sdfRadix = "2",
//         keepMSBorLSB = Array.fill(log2Up(256))(true),
//         minSRAMdepth = 64,
//         binPoint = 10
//       ),
//       dopplerFFTParams = FFTParams.fixed(
//         dataWidth = 16,
//         twiddleWidth = 16,
//         numPoints = 32,
//         useBitReverse  = true,
//         runTime = true,
//         numAddPipes = 1,
//         numMulPipes = 1,
//         use4Muls = true,
//         trimType = Convergent,
//         sdfRadix = "2",
//         expandLogic = Array.fill(log2Up(32))(0),
//         keepMSBorLSB = Array.fill(log2Up(32))(true),
//         minSRAMdepth = 32,
//         binPoint = 10
//       ),
//       zeroPadderRangeParams = Some(ZeroPadderParams(
//                                       proto = FixedPoint(16.W, 10.BP),
//                                       packetSizeStart = 256,
//                                       packetSizeEnd  = 256, // range FFT size
//                                       numberOfPackets = 32*3,
//                                       useQueue = false,
//                                       isDataComplex = true
//                                     )
//                                   ),
//       zeroPadderDopplerParams = Some(ZeroPadderParams(
//                                       proto = FixedPoint(16.W, 10.BP),
//                                       packetSizeStart = 32,
//                                       packetSizeEnd  = 32,
//                                       numberOfPackets = 256*3*4,  // this highly depends on number of outputNodes, if it is equal to 1 then then it is rangeFFTSize * numTxs * numRxs
//                                       useQueue = false,
//                                       isDataComplex = true
//                                     )
//                                   ),
//       fft2ControlParams  = FFT2RDControlParams(rangeFFTSize = 256, dopplerFFTSize = 32, numTxs = 3, numRxs = 4, outputNodes = 1, pingPong = false),
//       fft2ControlAddress = AddressSet(0x00000, 0xFFF),
//       rangeFFTAddress    = AddressSet(0x01000, 0xFFF),
//       dopplerFFTAddress  = AddressSet(0x02000, 0xFFF),
//       zeroPadderRangeAddress  = Some(AddressSet(0x03000, 0xFFF)),
//       zeroPadderDopplerAddress = Some(AddressSet(0x04000, 0xFFF))
//      ),
//     totalData = 256*32*3,
//     beatBytes = 4,
//     rawDataMuxAddress  = AddressSet(0x5000, 0xFF),
//     storeRawMemAddress = AddressSet(0x6000, 0xFF)
//   )
//   implicit val p: Parameters = Parameters.empty
//
//   val fft2Module = LazyModule(new AXI4StreamFFT2RDWithTestStructuresBlock(paramsFFT2RDTS, beatBytes = 4) with AXI4FFT2RDWithTestStructuresStandaloneBlock)
//   chisel3.Driver.execute(args, ()=> fft2Module.module)
// }
