package fft2rd

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class AXI4StreamAdapter1to4(val beatBytes: Int) extends LazyModule()(Parameters.empty) {
  val streamNode = AXI4StreamNexusNode(
    masterFn = (ms: Seq[AXI4StreamMasterPortParameters]) =>
      AXI4StreamMasterPortParameters(
        Seq(
          AXI4StreamMasterParameters(
            n = 4 // 4 * 8 is equal to 32
          )
        )
      ),
    slaveFn = ss => { AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _)) }
  )
  lazy val module = new LazyModuleImp(this) {
    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip
    // require that ins is 1
    val cntValidIn = RegInit(0.U(log2Up(outs.length).W))
    val inDataRegs = RegInit(VecInit(Seq.fill(outs.length)(0.U(32.W))))
    ins(0).ready := outs.map(c => c.ready).foldLeft(true.B)(_ && _)

    when(ins(0).ready && ins(0).valid) {
      cntValidIn := cntValidIn + 1.U
      inDataRegs(cntValidIn) := ins(0).bits.data
    }

    for ((out, index) <- outs.zipWithIndex) {
      out.valid := cntValidIn === (outs.length - 1).U && (ins(0).valid && ins(0).ready)
      if (index == (outs.length - 1)) {
        out.bits.data := ins(0).bits.data
      } else {
        out.bits.data := inDataRegs(index)
      }
      out.bits.last := (cntValidIn === (outs.length - 1).U) && ins(0).bits.last && (ins(0).valid && ins(0).ready)
    }
  }
}

trait AXI4StreamAdapter1to4StandaloneBlock extends AXI4StreamAdapter1to4 {

  val nOut = 4
  val outs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until nOut) yield {
    implicit val valName = ValName(s"outIO_$o")
    val out = BundleBridgeSink[AXI4StreamBundle]()
    out := AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) := streamNode

    InModuleBody { out.makeIO() }
  }
  val nIn = 1
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    streamNode := BundleBridgeToAXI4Stream(
      AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))
    ) := in
    InModuleBody { in.makeIO() }
  }
}

object AXI4StreamAdapter1to4App extends App {
  implicit val p: Parameters = Parameters.empty
  val lazyDut = LazyModule(new AXI4StreamAdapter1to4(beatBytes = 4) with AXI4StreamAdapter1to4StandaloneBlock)

  (new ChiselStage).execute(
    Array("--target-dir", "verilog/AXI4StreamAdapter1to4"),
    Seq(ChiselGeneratorAnnotation(() => lazyDut.module))
  )
}
