package fft2rd

import chisel3._
import chisel3.util._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

import org.chipsalliance.cde.config.Parameters
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

trait AXI4FFT2RDControlStandaloneBlock extends AXI4StreamFFT2RDControlBlock {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)

  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
    BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
    ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val nOut = params.outputNodes
  val outs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until nOut) yield {
    implicit val valName = ValName(s"outIO_$o")
    val out = BundleBridgeSink[AXI4StreamBundle]()
    out := AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) := streamNode

    InModuleBody { out.makeIO() }
  }
  val nIn = params.numRxs
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
     streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) := in
    InModuleBody { in.makeIO() }
  }
}

abstract class FFT2RDControlBlock [D, U, E, O, B <: Data] (params: FFT2RDControlParams, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val streamNode = AXI4StreamNexusNode(
    masterFn = (ms: Seq[AXI4StreamMasterPortParameters]) => AXI4StreamMasterPortParameters(
      Seq(AXI4StreamMasterParameters(
        n = 4 // 4 * 8 equal to 32
    ))),
    slaveFn = ss => {AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _))}
  )

  lazy val module = new LazyModuleImp(this) {

    //require() // !!! define this !!!

    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip

    val in = ins(0)
    val out = outs(0)

    val rangeFFTSize = params.rangeFFTSize
    val dopplerFFTSize = params.dopplerFFTSize
    val radarDataMatrixSize = rangeFFTSize*dopplerFFTSize
    val numChirps = RegInit(dopplerFFTSize.U(log2Ceil(dopplerFFTSize + 1).W))
    val numSamples = RegInit(rangeFFTSize.U(log2Ceil(rangeFFTSize + 1).W))
    val dopplerSize = RegInit(dopplerFFTSize.U(log2Ceil(dopplerFFTSize + 1).W))
    val numTxs = RegInit(params.numTxs.U(log2Ceil(params.numTxs + 1).W))
    val activeRangeSamples = RegInit(rangeFFTSize.U(log2Ceil(rangeFFTSize + 1).W))
    val numRxs = RegInit(params.numRxs.U(log2Ceil(params.numRxs + 1).W))

    val cntSamplesRange = RegInit(0.U(log2Ceil(rangeFFTSize).W))
    val cntChirpsRange = RegInit(0.U(log2Ceil(dopplerFFTSize).W))
    val cntSamplesDoppler = RegInit(0.U(log2Ceil(dopplerFFTSize).W))
    val cntRepeatDoppler = RegInit(0.U(log2Ceil(rangeFFTSize).W))
    val cntTxs = RegInit(0.U(log2Ceil(params.numTxs+1).W)) // what if numTxs is equal to 1
    val writeAddress = WireDefault(0.U((log2Ceil(radarDataMatrixSize)).W))
    val readAddress =  RegInit(0.U((log2Ceil(radarDataMatrixSize)).W))

    val startRead = RegInit(false.B)
    val finishRead = RegInit(false.B)
    val lastFlag = RegInit(false.B)
    val frameSize = (params.numTxs * params.numRxs)/params.outputNodes
    val frameCounter = RegInit(0.U(log2Ceil(frameSize + 1).W)) // frame size should be renamed, the name is not intuitive at all

    var fields = Seq(
      // settable registers
      RegField(log2Ceil(rangeFFTSize + 1), numSamples,
        RegFieldDesc(name = "numSamples", desc = "Configure number of samples inside chirp")),
      RegField(log2Ceil(dopplerFFTSize + 1), numChirps,
        RegFieldDesc(name = "numChirps", desc = "Configure number of chirps")),      // important for writing data into memory, when to stop
      RegField(log2Ceil(dopplerFFTSize + 1), dopplerSize,
        RegFieldDesc(name = "dopplerFFTSize", desc = "Configure doppler FFT size")), // important for reading
      RegField(log2Ceil(rangeFFTSize + 1), activeRangeSamples, // should be used instead of numSamples on Doppler side
        RegFieldDesc(name = "activeRangeSamples", desc = "Configure number of active range samples that should be used later for Doppler FFT")),
      RegField(log2Ceil(params.numTxs + 1), numTxs,
        RegFieldDesc(name = "numTxs", desc = "Configure number of transmitters")),
      RegField(log2Ceil(params.numRxs + 1), numRxs,
        RegFieldDesc(name = "numRxs", desc = "Configure number of active receivers"))
    )

    val readDir = if (params.outputNodes == 1 && params.incRegXYZorXZY.getOrElse(false)) Some(RegInit(false.B)) else None

    if (params.outputNodes == 1 && params.incRegXYZorXZY.getOrElse(false)) {
      fields = fields :+ RegField(1, readDir.get,
              RegFieldDesc(name = "readDirection", desc = "Defines reading out direction"))
    }


    regmap(
      fields.zipWithIndex.map({ case (f, i) =>
        i * beatBytes -> Seq(f)
      }): _*
    )

    val activeRxs = Wire(Vec(params.numRxs, Bool()))
    activeRxs.zipWithIndex.map { case (active, index) => {
        active := Mux(numRxs >= index.U, true.B, false.B)
      }
    }

    val protoData = SInt(32.W)
    // instatiate numRx times numTx memories
    val numMems = params.numTxs * params.numRxs
    val syncReadMems0 = Seq.fill(numMems)(SyncReadMem(rangeFFTSize*dopplerFFTSize, protoData))
    val syncReadMems1 = if (params.pingPong) Some(Seq.fill(numMems)(SyncReadMem(rangeFFTSize*dopplerFFTSize, protoData))) else None

    val pingPongWrite = if (params.pingPong) Some(RegInit(false.B)) else None
    val pingPongRead = if (params.pingPong) Some(RegInit(false.B)) else None

    val pingPongWritePrev = if (params.pingPong) Some(RegInit(false.B)) else None
    val pingPongReadPrev = if (params.pingPong) Some(RegInit(false.B)) else None
    val inReady = if (params.pingPong) Some(WireDefault(true.B)) else None
    val forbidWrite = if (params.pingPong) Some(WireDefault(false.B)) else None
    val forbidWritePrev = if (params.pingPong) Some(RegInit(false.B)) else None

    if (params.pingPong) {
      pingPongWritePrev.get := pingPongWrite.get
      pingPongReadPrev.get  := pingPongRead.get
      val case0 = ~pingPongWrite.get && ~pingPongRead.get && pingPongWritePrev.get && ~pingPongReadPrev.get
      val case1 = pingPongWrite.get && pingPongRead.get && ~pingPongWritePrev.get && pingPongReadPrev.get
      forbidWritePrev.get := forbidWrite.get

      when (case0 || case1) {
        forbidWrite.get := true.B
      }
      .elsewhen (pingPongRead.get =/= pingPongWrite.get) {
        forbidWrite.get := false.B
      }
      .otherwise {
        forbidWrite.get := forbidWritePrev.get
      }
      inReady.get := ~forbidWrite.get
    }


    val inFire = ins.zip(activeRxs).map{ case (in, active) => Mux(active, in.valid & in.ready, true.B) }.reduce((a,b) => a & b)

    when (inFire && cntSamplesRange === (numSamples - 1.U)) {
      cntSamplesRange := 0.U

      when (cntTxs =/= numTxs-1.U) {
        cntTxs := cntTxs + 1.U
      }
      .otherwise {
        cntTxs := 0.U
      }

      when (cntChirpsRange === (numChirps - 1.U) && (cntTxs === numTxs-1.U)) { // can be simplified
        cntChirpsRange := 0.U
        if (params.pingPong) {
          pingPongWrite.get := ~pingPongWrite.get
        }
        // if we do not have ping-pong then here we can say, start to read data
        startRead := true.B // make it optional!!!
      }
      .elsewhen(cntTxs === numTxs -1.U) {
        cntChirpsRange := cntChirpsRange + 1.U
      }
    }
    .elsewhen (inFire) {
      cntSamplesRange := cntSamplesRange + 1.U
    }

    if (params.addressGenDir) {
      writeAddress := cntSamplesRange + cntChirpsRange*numSamples
    }
    else {
      writeAddress := cntChirpsRange + cntSamplesRange*numChirps
    }


    val numTxIndices = Wire(Vec(params.numTxs, UInt(log2Ceil(params.numTxs + 1).W)))
    numTxIndices.zipWithIndex.map { case (txIndex, index) => {
        txIndex := index.U
      }
    }

    numTxIndices.zipWithIndex.foreach { case (txIndex, index) =>
      //println(index)
      for (w <- index*params.numRxs until (index+1)*(params.numRxs))
      {
        println(w)
        val inIdx = w % params.numRxs
        when (inFire) {
          when (cntTxs === txIndex) {
            if (params.pingPong) {
              when (pingPongWrite.get === false.B) {
                syncReadMems0(w)(writeAddress) := ins(inIdx).bits.data.asTypeOf(protoData)
              }
              .otherwise {
                syncReadMems1.get(w)(writeAddress) := ins(inIdx).bits.data.asTypeOf(protoData)
              }
            }
            else {
              syncReadMems0(w)(writeAddress) := ins(inIdx).bits.data.asTypeOf(protoData)
            }
          }
        }
        // for the version without ping pong it is ~startRead
        if (params.pingPong) {
          ins(inIdx).ready := inReady.get
        }
        else {
          ins(inIdx).ready := ~startRead
        }
          //ins(inIdx).ready := true.B // should be checked! pingpongWrite and pingPongRead should be checked for in.ready generation - if pingpongRead is equal to pingpongwrite then in.ready should be disabled
      }
    }

    if (params.outputNodes > 1) {
      when (((pingPongRead.getOrElse(startRead) =/= pingPongWrite.getOrElse(false.B)) || forbidWrite.getOrElse(false.B)) && out.ready === true.B) {
        when (cntSamplesDoppler === (numChirps - 1.U) && out.ready === true.B) {
          cntSamplesDoppler := 0.U
          //when (cntRepeatDoppler === (numSamples - 1.U)) { // most probably should be replaced with activeRangeSamples
          when (cntRepeatDoppler === (activeRangeSamples - 1.U)) {
            cntRepeatDoppler := 0.U
            when (frameCounter === (frameSize.U - 1.U)) {
              frameCounter := 0.U
              if (params.pingPong == true) {
                pingPongRead.get := ~pingPongRead.get
              }
              lastFlag := true.B
              startRead := false.B
            }
            .otherwise {
              frameCounter := frameCounter + 1.U
            }
          }
          .otherwise {
            lastFlag := false.B
            cntRepeatDoppler := cntRepeatDoppler + 1.U
          }
        }
        .otherwise {
          lastFlag := false.B
          cntSamplesDoppler := cntSamplesDoppler + 1.U
        }
      }
      .otherwise {
        lastFlag := false.B
      }
    }
    else {
      when ((pingPongRead.getOrElse(startRead) =/= pingPongWrite.getOrElse(false.B) || forbidWrite.getOrElse(false.B)) && out.ready === true.B) {
        when (cntSamplesDoppler === (numChirps - 1.U) && out.ready === true.B) {
          // if/else can be moved here
          cntSamplesDoppler := 0.U
          when (readDir.getOrElse((params.readXYZorXZY.get).B)) {
            when (cntRepeatDoppler === (activeRangeSamples - 1.U)) {
              cntRepeatDoppler := 0.U
              when (frameCounter === (frameSize.U - 1.U)) {
                frameCounter := 0.U
                if (params.pingPong == true) {
                  pingPongRead.get := ~pingPongRead.get
                }
                lastFlag := true.B
                startRead := false.B
              }
              .otherwise {
                frameCounter := frameCounter + 1.U
              }
            }
            .otherwise {
              lastFlag := false.B
              cntRepeatDoppler := cntRepeatDoppler + 1.U
            }
          }
          .otherwise {
            when (frameCounter === (params.numTxs*params.numRxs - 1).U) {
              frameCounter := 0.U
              when (cntRepeatDoppler === (activeRangeSamples - 1.U)) {
                cntRepeatDoppler := 0.U
                if (params.pingPong == true) {
                  pingPongRead.get := ~pingPongRead.get
                }
                lastFlag := true.B
                startRead := false.B
              }
              .otherwise {
                cntRepeatDoppler := cntRepeatDoppler + 1.U
                lastFlag := false.B
              }
            }
            .otherwise {
              frameCounter := frameCounter + 1.U
              lastFlag := false.B
            }
          }
        }
        .otherwise {
          lastFlag := false.B
          cntSamplesDoppler := cntSamplesDoppler + 1.U
        }
      }
      .otherwise {
        lastFlag := false.B
      }
    }

//     if ((params.outputNodes > 1) || params.readXYZorXZY.getOrElse(true)) {
//       when (((pingPongRead.getOrElse(startRead) =/= pingPongWrite.getOrElse(false.B)) || forbidWrite.getOrElse(false.B)) && out.ready === true.B) {
//         when (cntSamplesDoppler === (numChirps - 1.U) && out.ready === true.B) {
//           cntSamplesDoppler := 0.U
//           //when (cntRepeatDoppler === (numSamples - 1.U)) { // most probably should be replaced with activeRangeSamples
//           when (cntRepeatDoppler === (activeRangeSamples - 1.U)) {
//             cntRepeatDoppler := 0.U
//             when (frameCounter === (frameSize.U - 1.U)) {
//               frameCounter := 0.U
//               if (params.pingPong == true) {
//                 pingPongRead.get := ~pingPongRead.get
//               }
//               lastFlag := true.B
//               startRead := false.B
//             }
//             .otherwise {
//               frameCounter := frameCounter + 1.U
//             }
//           }
//           .otherwise {
//             lastFlag := false.B
//             cntRepeatDoppler := cntRepeatDoppler + 1.U
//           }
//         }
//         .otherwise {
//           lastFlag := false.B
//           cntSamplesDoppler := cntSamplesDoppler + 1.U
//         }
//       }
//       .otherwise {
//         lastFlag := false.B
//       }
//     }
//     else {
//       when ((pingPongRead.getOrElse(startRead) =/= pingPongWrite.getOrElse(false.B) || forbidWrite.getOrElse(false.B)) && out.ready === true.B) {
//         when (cntSamplesDoppler === (numChirps - 1.U) && out.ready === true.B) {
//           // if/else can be moved here
//           cntSamplesDoppler := 0.U
//           when (frameCounter === (params.numTxs*params.numRxs - 1).U) {
//             frameCounter := 0.U
//             when (cntRepeatDoppler === (activeRangeSamples - 1.U)) {
//               cntRepeatDoppler := 0.U
//               if (params.pingPong == true) {
//                 pingPongRead.get := ~pingPongRead.get
//               }
//               lastFlag := true.B
//               startRead := false.B
//             }
//             .otherwise {
//               cntRepeatDoppler := cntRepeatDoppler + 1.U
//               lastFlag := false.B
//             }
//           }
//           .otherwise {
//             frameCounter := frameCounter + 1.U
//             lastFlag := false.B
//           }
//         }
//         .otherwise {
//           lastFlag := false.B
//           cntSamplesDoppler := cntSamplesDoppler + 1.U
//         }
//       }
//       .otherwise {
//         lastFlag := false.B
//       }
//     }

    if (params.addressGenDir) {
      readAddress := cntRepeatDoppler + cntSamplesDoppler * numSamples // this should not be changed, cntRepeatDopppler will count until activeRangeSamples
    }
    else {
      readAddress := cntSamplesDoppler + cntRepeatDoppler * numChirps
    }

    val readAddress0 = Wire(Vec(params.numRxs*params.numTxs, readAddress.cloneType))
    readAddress0.map { case (readAddr) => readAddr := readAddress  }
    val outsData = Wire(Vec(params.outputNodes, protoData))

    val readAddress1 = if (params.pingPong) Some(Wire(Vec(params.numRxs*params.numTxs, readAddress.cloneType))) else None
    if (params.pingPong) {
      readAddress1.get.map { case (readAddr) => readAddr := readAddress }
    }

    if (params.outputNodes == 1) {
      val cases = if (params.pingPong == false) (0 until params.numTxs*params.numRxs).map( c => c.U).zipWithIndex.map { case (txIndex, indexTx) => //maybe some renaming will be applied here
          (txIndex === RegNext(RegNext(frameCounter))) ->  syncReadMems0((indexTx))(readAddress0((indexTx))) }
        else
          (0 until params.numTxs*params.numRxs).map( c => c.U).zipWithIndex.map { case (txIndex, indexTx) =>
          //(txIndex === frameCounter) ->  Mux(RegNext(RegNext(pingPongRead.get, false.B)),
          //                                  syncReadMems1.get((indexTx))(readAddress1.get((indexTx))),
          //                                  syncReadMems0((indexTx))(readAddress0((indexTx)))) }
          (txIndex === RegNext(RegNext(frameCounter))) ->  Mux(RegNext(RegNext(pingPongRead.get, false.B)),
                                            syncReadMems1.get((indexTx))(readAddress1.get((indexTx))),
                                            syncReadMems0((indexTx))(readAddress0((indexTx)))) }

      outsData(0) := MuxCase(0.S(32.W), cases)

      dontTouch(outsData(0))
      outsData(0).suggestName("outsData0")
    }
    else if (params.outputNodes == params.numRxs) {
      for (i <- 0 until params.numRxs) {
        val cases = if (params.pingPong == false) numTxIndices.zipWithIndex.map { case (txIndex, indexTx) =>
            (txIndex === RegNext(RegNext(frameCounter))) ->  syncReadMems0((indexTx*params.numRxs + i))(readAddress0((indexTx*params.numRxs + i))) }
            //(txIndex === frameCounter) ->  syncReadMems0((indexTx*params.numRxs + i))(readAddress0((indexTx*params.numRxs + i))) }
          else
            numTxIndices.zipWithIndex.map { case (txIndex, indexTx) =>
            //(txIndex === frameCounter) ->  Mux(RegNext(RegNext(pingPongRead.get, false.B)),
            //                                   syncReadMems1.get((indexTx*params.numRxs + i))(readAddress1.get((indexTx*params.numRxs + i))),
            //                                   syncReadMems0((indexTx*params.numRxs + i))(readAddress0((indexTx*params.numRxs + i))))  }
            (txIndex === RegNext(RegNext(frameCounter))) ->  Mux(RegNext(RegNext(pingPongRead.get, false.B)),
                                               syncReadMems1.get((indexTx*params.numRxs + i))(readAddress1.get((indexTx*params.numRxs + i))),
                                               syncReadMems0((indexTx*params.numRxs + i))(readAddress0((indexTx*params.numRxs + i))))  }

        outsData(i) := MuxCase(0.S(32.W), cases)
      }
    }
    else if (params.outputNodes == (params.numTxs * params.numRxs)) {
      for (i <- 0 until params.outputNodes) {
        if (params.pingPong == false) { // no need to change anything this should work!
          outsData(i) := syncReadMems0(i)(readAddress0(i))
        }
        else {
          outsData(i) := Mux(RegNext(RegNext(pingPongRead.get, false.B)), syncReadMems1.get(i)(readAddress1.get(i)), syncReadMems0(i)(readAddress0(i)))
        }
      }
    }

    val validOut = if (params.pingPong)  RegNext(RegNext(((pingPongRead.get =/= pingPongWrite.get) || forbidWrite.get), false.B)) else RegNext(RegNext(startRead))
    outs.zipWithIndex.map { case (out, i) =>
      val outQueue =  Module(new Queue(chiselTypeOf(out.bits.data), entries = 3, pipe = true, flow = false))    // pipe = true, flow = true))
      outQueue.io.enq.bits := outsData(i).asTypeOf(out.bits.data)
      outQueue.io.enq.valid := validOut && RegNext(RegNext(out.ready)) //validOut && RegNext(RegNext(startRead))
      outQueue.io.deq.ready := out.ready
      out.bits.data  := outQueue.io.deq.bits
      out.valid      := outQueue.io.deq.valid

      // generate last flag after each radar data cube!
      val outQueueLast = Module(new Queue(chiselTypeOf(out.bits.last), entries = 3, pipe = true, flow = false)) //pipe = true, flow = true))
      outQueueLast.io.enq.bits  := RegNext(lastFlag, false.B)
      outQueueLast.io.enq.valid := RegNext(RegNext(startRead && out.ready))
      outQueueLast.io.deq.ready := out.ready
      out.bits.last := outQueueLast.io.deq.bits
    }
  }
}

class AXI4StreamFFT2RDControlBlock(val params: FFT2RDControlParams, address: AddressSet, val beatBytes: Int = 4) (implicit p: Parameters) extends FFT2RDControlBlock[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}

object FFT2RDControlDspBlockAXI4 extends App
{
  //val paramsFFT2RDControl: FFT2RDControlParams = FFT2RDControlParams(rangeFFTSize = 256, dopplerFFTSize = 32, numRxs = 8, numTxs = 4, outputNodes = 32, pingPong = true)
  val paramsFFT2RDControl: FFT2RDControlParams = FFT2RDControlParams(rangeFFTSize = 256, dopplerFFTSize = 128, numRxs = 8, numTxs = 9, outputNodes = 72, pingPong = true)
  implicit val p: Parameters = Parameters.empty

  val fft2Module = LazyModule(new AXI4StreamFFT2RDControlBlock(paramsFFT2RDControl, AddressSet(0x00000, 0xFF), beatBytes = 4) with AXI4FFT2RDControlStandaloneBlock)
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => fft2Module.module)))

}
