package fft2rd

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

class AXI4StreamOutIO extends Bundle {
  val valid_out = Output(Bool())
  val last_out  = Output(Bool())
  val ready_in  = Input(Bool())
  val data_out  = Output(UInt(32.W))
  //val data_out  = Output(UInt((beatBytes*8).W))
}

class AXI4StreamInIO extends Bundle {
  val valid_in = Input(Bool())
  val last_in  = Input(Bool())
  val ready_out  = Output(Bool())
 // val data_in  = Input(UInt((beatBytes*8).W))
  val data_in  = Input(UInt(32.W))
}

trait AXI4StreamMultipleSlaveWrapperPins extends AXI4StreamMultipleSlaveWrapper {
  val nIn = 4
  val inIO: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
      slaveNode(i) := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) := in
    InModuleBody { in.makeIO() }
  }
}

class AXI4StreamMultipleSlaveWrapper(val beatBytes: Int = 4, val numStreams: Int = 4) extends LazyModule()(Parameters.empty) {
  val slaveParams = AXI4StreamSlaveParameters()
  val slaveNode = Seq.fill(numStreams)(AXI4StreamSlaveNode(slaveParams))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(Vec(numStreams, new AXI4StreamOutIO))
    for (i <- 0 until numStreams) {
      val (in, _)  = slaveNode(i).in(0)
      io(i).valid_out := in.valid
      io(i).last_out := in.bits.last
      in.ready := io(i).ready_in
      io(i).data_out := in.bits.data
    }
  }
}

class AXI4StreamMultipleMasterWrapper(val beatBytes: Int = 4, val numStreams: Int = 4) extends LazyModule()(Parameters.empty) {
  val masterParams = AXI4StreamMasterParameters(
    name = "AXI4 Stream Master Wrapper",
    n = 4, // just 2*8 -> 16 bits
    numMasters = 1
  )
  val masterNode = Seq.fill(numStreams)(AXI4StreamMasterNode(masterParams))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(Vec(numStreams, new AXI4StreamInIO))
    for (i <- 0 until numStreams) {
      val (out, _)  = masterNode(i).out(0)
      out.valid := io(i).valid_in
      out.bits.last := io(i).last_in
      out.bits.data := io(i).data_in
      io(i).ready_out := out.ready
    }
  }
}

class AXI4StreamMultipleIdentityWrapper(val beatBytes: Int = 4, val numStreams: Int = 4) extends LazyModule()(Parameters.empty) {
  val streamNode = Seq.fill(numStreams)(AXI4StreamIdentityNode())

  lazy val module = new LazyModuleImp(this) {
    val io = IO(Vec(numStreams, new AXI4StreamOutIO))

    for (i <- 0 until numStreams) {
      val (in, _)  = streamNode(i).in(0)
      val (out, _) = streamNode(i).out(0)

      io(i).valid_out := in.valid
      io(i).last_out := in.bits.last
      io(i).data_out := in.bits.data

      out.valid := in.valid
      out.bits.last := in.bits.last
      out.bits.data := in.bits.data
      in.ready := out.ready
    }
  }
}


class AXI4StreamCopyInputStreams(val beatBytes: Int = 4, val numStreams: Int = 4) extends LazyModule()(Parameters.empty) {

  val slaveParams = AXI4StreamSlaveParameters()
  val slaveNode1 = AXI4StreamSlaveNode(slaveParams)
  val slaveNode2 = AXI4StreamSlaveNode(slaveParams)

  val masterParams = AXI4StreamMasterParameters(
    name = "AXI4 Stream Copy Input Streams",
    n = 4, // just 2*8 -> 16 bits
    numMasters = 1
  )
  val masterNode = Seq.fill(numStreams)(AXI4StreamMasterNode(masterParams))

  lazy val module = new LazyModuleImp(this) {
    val (in1, _)  = slaveNode1.in(0)
    val (in2, _)  = slaveNode2.in(0)

    for (i <- 0 until numStreams/2) {
      val (out, _)  = masterNode(i).out(0)
      out.valid := in1.valid
      out.bits := in1.bits
    }
    for (i <- numStreams/2 until numStreams) {
      val (out, _)  = masterNode(i).out(0)
      out.valid := in2.valid
      out.bits := in2.bits
    }

    val outs = masterNode.map { out => out.out(0) }
    val readyOuts = outs.map { case (out, _) => out.ready }
    val readyAND = readyOuts.reduce(_ && _)
    in1.ready := readyAND
    in2.ready := readyAND
  }
}
class AXI4Stream2InputMux(val beatBytes: Int = 4) extends LazyModule()(Parameters.empty) {

  val slaveParams = AXI4StreamSlaveParameters()
  val slaveNode1 = AXI4StreamSlaveNode(slaveParams)
  val slaveNode2 = AXI4StreamSlaveNode(slaveParams)

  val masterParams = AXI4StreamMasterParameters(
    name = "AXI4 Stream Simple Mux",
    n = beatBytes,
    numMasters = 1
  )
  val masterNode = AXI4StreamMasterNode(masterParams)

  lazy val module = new LazyModuleImp(this) {
    val sel = IO(Input(Bool()))

    val (out, _) = masterNode.out(0)
    val (in1, _) = slaveNode1.in(0)
    val (in2, _) = slaveNode2.in(0)
    in1.ready := out.ready
    in2.ready := out.ready

    when (sel) {
      out.valid := in2.valid
      out.bits := in2.bits
    }
    .otherwise {
      out.valid := in1.valid
      out.bits := in1.bits
    }
  }
}

class AXI4StreamSimpleMux(val numStreams: Int = 4, val beatBytes: Int = 4) extends LazyModule()(Parameters.empty) {

  val slaveParams = AXI4StreamSlaveParameters()
  val slaveNode1 = Seq.fill(numStreams)(AXI4StreamSlaveNode(slaveParams))
  val slaveNode2 = Seq.fill(numStreams)(AXI4StreamSlaveNode(slaveParams))

  val masterParams = AXI4StreamMasterParameters(
    name = "AXI4 Stream Simple Mux",
    n = beatBytes,
    numMasters = 1
  )
  val masterNode = Seq.fill(numStreams)(AXI4StreamMasterNode(masterParams))

  lazy val module = new LazyModuleImp(this) {
    val sel = IO(Input(Bool()))

    for (i <- 0 until numStreams) {
      val (out, _) = masterNode(i).out(0)
      val (in1, _) = slaveNode1(i).in(0)
      val (in2, _) = slaveNode2(i).in(0)

      when (sel) {
        out.valid := in2.valid
        out.bits := in2.bits
        in2.ready := out.ready
      }
      .otherwise {
        out.valid := in1.valid
        out.bits := in1.bits
        in1.ready := out.ready
      }
    }
  }
}

object AXI4StreamMultipleSlaveWrapperApp extends App
{
  val lazyDut = LazyModule(new AXI4StreamMultipleSlaveWrapper() with AXI4StreamMultipleSlaveWrapperPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/AXI4StreamMultipleSlaveWrapper"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
