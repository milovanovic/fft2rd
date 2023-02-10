package utils

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

//class BPMDemodulation[T <: Data : Real : BinaryRepresentation]
abstract class HeaderInserter [D, U, E, O, B <: Data] (mode1024: Boolean, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  
  val streamNode = AXI4StreamIdentityNode()
  
  lazy val module = new LazyModuleImp(this) {
    
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)
    
    val modeSelection = RegInit(Bool(), !mode1024.B)
    //val modeSelection = RegInit(Bool(), false.B)
    //val modeSelection = RegInit(Bool(), true.B)
    val chirpNum = RegInit(UInt(9.W), 32.U)
    val sampleNum = RegInit(UInt(11.W), 256.U)
    val antennaNum = RegInit(UInt(4.W), 12.U)
    
    val outCounter = RegInit(UInt(32.W), 0.U)
    val xDimensionCounter = RegInit(UInt(9.W), 0.U)
    val yDimensionCounter = RegInit(UInt(11.W), 0.U)
    val antennaCounter = RegInit(UInt(4.W), 0.U)
    val segmentCounter = RegInit(UInt(5.W), 0.U)
    val segmentCounter2 = RegInit(Bool(), false.B)
    val segmentCounter3 = RegInit(UInt(8.W), 0.U)
    
    val threshold = Wire(UInt(11.W))
    
    val outputDataLastVal = Wire(UInt(32.W))
    
    val fields = Seq(
      RegField(1, modeSelection,
        RegFieldDesc(name = "modeSelection", desc = "Mode Selection - false for 1024 bytes, true for 768 bytes")),
      RegField(9, chirpNum,
        RegFieldDesc(name = "chirpNum", desc = "Chirp number - number of chirps in a frame")),
      RegField(11, sampleNum,
        RegFieldDesc(name = "sampleNum", desc = "Sample number - number of samples in a chirp")),
      RegField(4, antennaNum,
        RegFieldDesc(name = "antennaNum", desc = "Antenna number - number of antennas used"))
    )
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
    
    threshold := Mux(modeSelection, 768.U >> 2, 1024.U >> 2)
    
    when(out.fire && (outCounter === threshold)) {
        outCounter := 0.U
    } .elsewhen(out.fire && (outCounter < threshold)) {
        outCounter := outCounter + 1.U
    }
    
    when(out.fire && (outCounter === threshold) && (segmentCounter === 31.U)) {
        segmentCounter := 0.U
    } .elsewhen(out.fire && (outCounter === threshold)  && (segmentCounter < 31.U)) {
        segmentCounter := segmentCounter + 1.U
    }
    
    when(in.fire && (xDimensionCounter === (chirpNum - 1.U))) {
        xDimensionCounter := 0.U
    } .elsewhen(in.fire && (xDimensionCounter < (chirpNum - 1.U))) {
        xDimensionCounter := xDimensionCounter + 1.U
    }
    
    when(in.fire && (xDimensionCounter === (chirpNum - 1.U)) && (yDimensionCounter === (sampleNum - 1.U))) {
        yDimensionCounter := 0.U
    } .elsewhen(in.fire && (xDimensionCounter === (chirpNum - 1.U)) && (yDimensionCounter < (sampleNum - 1.U))){
        yDimensionCounter := yDimensionCounter + 1.U
    }
    
    when(in.fire && (xDimensionCounter === (chirpNum - 1.U)) && (yDimensionCounter === (sampleNum - 1.U)) && (antennaCounter === (antennaNum - 1.U))) {
        antennaCounter := 0.U
    } .elsewhen(in.fire && (xDimensionCounter === (chirpNum - 1.U)) && (yDimensionCounter === (sampleNum - 1.U)) && (antennaCounter < (antennaNum - 1.U))){
        antennaCounter := antennaCounter + 1.U
    }
    
    when(out.fire && (outCounter === 0.U)) {
      segmentCounter2 := !segmentCounter2
    }
    
    when(out.fire && (outCounter === 0.U) && (segmentCounter2 === true.B)) {
      segmentCounter3 := segmentCounter3 + 1.U
    }
    
    
    when(!modeSelection) {
      outputDataLastVal := Cat(Cat(0.U(8.W), 165.U(8.W)), Cat(antennaCounter.asTypeOf(UInt(8.W)), segmentCounter.asTypeOf(UInt(8.W))))
    } .otherwise {
      outputDataLastVal := Cat(Cat(0.U(8.W), 165.U(8.W)), Cat(segmentCounter2.asTypeOf(UInt(8.W)), segmentCounter3.asTypeOf(UInt(8.W))))
    }
    
    in.ready := out.ready && (outCounter =/= 0.U)
    out.bits.last := out.fire && (outCounter === threshold)
    //out.bits.last := out.fire && (xDimensionCounter === (chirpNum - 1.U)) && (yDimensionCounter === (sampleNum - 1.U)) && (antennaCounter === (antennaNum - 1.U)) // not sure about this one
    //out.valid := (in.valid || (outCounter === 0.U)) && out.ready
    out.valid := in.valid && out.ready
    out.bits.data := Mux((outCounter === 0.U), outputDataLastVal, in.bits.data) 
    
  }
  
}

class AXI4HeaderInserterBlock(mode1024: Boolean, address: AddressSet,  _beatBytes: Int = 4)(implicit p: Parameters) extends HeaderInserter[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](mode1024, _beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes))
}

trait HeaderInserterPins extends AXI4HeaderInserterBlock {
  def beatBytes: Int = 4
  
  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
  
  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode
  
  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
  
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}
}

object HeaderInserterApp extends App {

  val beatBytes = 4
  val baseAddress = 0x500
  val mode1024 = true
  
  implicit val p: Parameters = Parameters.empty
  val lazyDut = LazyModule(new AXI4HeaderInserterBlock(mode1024, AddressSet(baseAddress, 0xFF), beatBytes) with HeaderInserterPins {})

  (new ChiselStage).execute(Array("--target-dir", "verilog/HeaderInserter"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
