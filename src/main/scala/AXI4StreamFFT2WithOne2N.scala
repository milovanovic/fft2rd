// package fft2rd
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
//
// //import dsputils._
//
// trait AXI4FFT2WithOne2NStandaloneBlock extends AXI4StreamFFT2WithOne2NBlock[FixedPoint] {
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
//   val nOut = params.fft2ControlParams.outputNodes
//   val outs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until nOut) yield {
//     implicit val valName = ValName(s"outIO_$o")
//     val out = BundleBridgeSink[AXI4StreamBundle]()
//     out := AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) := dopplerFFT.streamNode
//     InModuleBody { out.makeIO() }
//   }
//
//   val ioInNode  = Seq.fill(streamNodeIn.length) { BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes/2))) }
//   (streamNodeIn, ioInNode).zipped.map{ (out, in) => { out := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes/2)) := in }}
// /* 1D input streamNode pins */
//   ioInNode.zipWithIndex.map{ case (m, i) => {
//       implicit val valName = ValName(s"in1D${i}")
//       val in_1D = InModuleBody { m.makeIO() }
//     }
//   }
//
//
// //   val nIn = params.fft2ControlParams.numRxs
// //   val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
// //     implicit val valName = ValName(s"inIO_$i")
// //     val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
// //
// //     streamNodeIn(i) :=  BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := in
// // //     if (params.zeroPadderRangeParams != None) {
// // //       zeroPadderRange.get.streamNode :=  BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n    = beatBytes))) := in
// // //     }
// // //     else {
// // //       rangeFFT.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) := in
// // //     }
// //
// //     InModuleBody { in.makeIO() }
// //   }
// }
//
// class AXI4StreamFFT2WithOne2NBlock [T <: Data : Real: BinaryRepresentation] (val params: FFT2Params[T], val beatBytes: Int) extends LazyModule()(Parameters.empty) {
//   // number of chirps
//   val streamNodeIn = Seq.fill(params.fft2ControlParams.numRxs)(AXI4StreamWidthAdapter.nToOne(2))
//
//   val fft2Control = LazyModule(new AXI4StreamFFT2ControlBlock(params.fft2ControlParams, params.fft2ControlAddress, beatBytes = beatBytes))
//   val rangeFFT    = LazyModule(new AXI4MultipleFFTsBlock(params.rangeFFTParams, params.rangeFFTAddress, _beatBytes = beatBytes, configInterface = false))
//   val dopplerFFT  = LazyModule(new AXI4MultipleFFTsBlock(params.dopplerFFTParams, params.dopplerFFTAddress, _beatBytes = beatBytes, configInterface = false))
//   val zeroPadderRange = if (params.zeroPadderRangeParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.zeroPadderRangeParams.get, params.zeroPadderRangeAddress.get))) else None
//   val zeroPadderDoppler = if (params.zeroPadderDopplerParams != None) Some(LazyModule(new AXI4MultipleZeroPadders(params.zeroPadderDopplerParams.get, params.zeroPadderDopplerAddress.get))) else None
//
//   //val zeroPadderDoppler = Some(LazyModule(new ))
//
//   for (i <- 0 until params.fft2ControlParams.numRxs) {
//     if (params.zeroPadderRangeParams != None) {
//       zeroPadderRange.get.streamNode := streamNodeIn(i)
//       rangeFFT.streamNode := zeroPadderRange.get.streamNode
//       fft2Control.streamNode := rangeFFT.streamNode   // AXI4StreamBuffer() - maybe not necessary to have - to be checked
//     }
//     else {
//       rangeFFT.streamNode := streamNodeIn(i)
//       fft2Control.streamNode := rangeFFT.streamNode   // AXI4StreamBuffer() - maybe not necessary to have - to be checked
//     }
//   }
//   for (i <- 0 until params.fft2ControlParams.outputNodes) {
//     if (params.zeroPadderDopplerParams != None) {
//       dopplerFFT.streamNode  := zeroPadderDoppler.get.streamNode
//       zeroPadderDoppler.get.streamNode := fft2Control.streamNode
//     }
//     else {
//       dopplerFFT.streamNode := fft2Control.streamNode // AXI4StreamBuffer()
//     }
//   }
//
//   var blocks = Seq(fft2Control, rangeFFT, dopplerFFT) // instead of lazy val, var is used
//   if (params.zeroPadderRangeParams != None) {
//     blocks = blocks :+ zeroPadderRange.get
//   }
//   if (params.zeroPadderDopplerParams != None) {
//     blocks = blocks :+ zeroPadderDoppler.get
//   }
//   //print(blocks.length)
//
//   val bus = LazyModule(new AXI4Xbar)
//   val mem = Some(bus.node)
//
//   for (b <- blocks) {
//     b.mem.foreach { _ := AXI4Buffer() := bus.node }
//   }
//
//   lazy val module = new LazyModuleImp(this) {}
// }
//
// object FFT2WithOneToNDspBlockAXI4 extends App
// {
//   val rangeFFTSize = 256
//   val dopplerFFTSize = 32
//   val numTxs = 3
//   val numRxs = 4
//
//   val paramsFFT2: FFT2Params[FixedPoint] = FFT2Params (
//       rangeFFTParams = FFTParams.fixed(
//         dataWidth = 12,
//         twiddleWidth = 16,
//         numPoints = rangeFFTSize,
//         useBitReverse  = true,
//         runTime = true,
//         numAddPipes = 1,
//         numMulPipes = 1,
//         use4Muls = true,
//         sdfRadix = "2",
//         trimType = Convergent,
//         expandLogic = Array.fill(log2Up(rangeFFTSize))(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
//         keepMSBorLSB = Array.fill(log2Up(rangeFFTSize))(true),
//         minSRAMdepth = 64,
//         binPoint = 10
//       ),
//       dopplerFFTParams = FFTParams.fixed(
//         dataWidth = 16,
//         twiddleWidth = 16,
//         numPoints = dopplerFFTSize,
//         useBitReverse  = true,
//         runTime = true,
//         numAddPipes = 1,
//         numMulPipes = 1,
//         use4Muls = true,
//         sdfRadix = "2",
//         trimType = Convergent,
//         expandLogic = Array.fill(log2Up(dopplerFFTSize))(0),
//         keepMSBorLSB = Array.fill(log2Up(dopplerFFTSize))(true),
//         minSRAMdepth = 8,
//         binPoint = 10
//       ),
//       fft2ControlParams  = FFT2ControlParams(rangeFFTSize = rangeFFTSize,
//                                             dopplerFFTSize = dopplerFFTSize,
//                                             numTxs = numTxs,
//                                             numRxs = numRxs,
//                                             outputNodes = 1,
//                                             pingPong = false,
//                                             readXYZorXZY = Some(false)),
//       // zeropadder parameters
//       zeroPadderRangeParams = Some(ZeroPadderParams(
//                                   proto = FixedPoint(16.W, 10.BP),
//                                   packetSizeStart = 256,
//                                   packetSizeEnd  = 256,       // range FFT size
//                                   numberOfPackets = 32*3,     // dopplerFFTSize * numTxs
//                                   useQueue = false,
//                                   isDataComplex = true
//                                 )
//                               ),
//       zeroPadderDopplerParams = Some(ZeroPadderParams(
//                                   proto = FixedPoint(16.W, 10.BP),
//                                   packetSizeStart = 32,
//                                   packetSizeEnd  = 32,
//                                   numberOfPackets = 256*3*4,  // this highly depends on number of outputNodes!, if it is equal to 1 then then it is rangeFFTSize * numTxs * numRxs
//                                   useQueue = false,
//                                   isDataComplex = true
//                                 )
//                               ),
//       zeroPadderRangeAddress  = Some(AddressSet(0x03000, 0xFFF)),
//       zeroPadderDopplerAddress = Some(AddressSet(0x04000, 0xFFF)),
//       fft2ControlAddress = AddressSet(0x00000, 0xFFF),
//       rangeFFTAddress    = AddressSet(0x01000, 0xFFF),
//       dopplerFFTAddress  = AddressSet(0x02000, 0xFFF)
//     )
//   implicit val p: Parameters = Parameters.empty
//
//   val fft2Module = LazyModule(new AXI4StreamFFT2WithOne2NBlock(paramsFFT2, beatBytes = 4) with AXI4FFT2WithOne2NStandaloneBlock)
//   chisel3.Driver.execute(args, ()=> fft2Module.module)
// }
