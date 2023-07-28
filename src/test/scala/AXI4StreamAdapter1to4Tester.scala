package fft2rd

import chisel3._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._

import chisel3.iotesters.PeekPokeTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.chipsalliance.cde.config.Parameters

import scala.util.{Random}

class AXI4StreamAdapter1to4Tester (
  dut: AXI4StreamAdapter1to4 with AXI4StreamAdapter1to4StandaloneBlock,
  beatBytes: Int = 4,
  sizeOfInputVector: Int = 8,
) extends PeekPokeTester(dut.module) {

  Random.setSeed(11110L)
  val inData = Seq.fill(sizeOfInputVector)(Random.nextInt(1<<(beatBytes*2)).toDouble)
  var cntIn = 0
  var cntOut = 0
  var inValid = 0
  val sizeOfOutputVector = (sizeOfInputVector/4).toInt
  var outReady = 0
  var peekedVal0: BigInt = 0
  var peekedVal1: BigInt = 0
  var peekedVal2: BigInt = 0
  var peekedVal3: BigInt = 0

  while (cntIn < sizeOfInputVector || cntOut < sizeOfOutputVector) {
    outReady = Random.nextInt(2)
    poke(dut.outs(0).ready, outReady)
    poke(dut.outs(1).ready, outReady)
    poke(dut.outs(2).ready, outReady)
    poke(dut.outs(3).ready, outReady)

    if (cntIn < sizeOfInputVector) {
      inValid = Random.nextInt(2)
      poke(dut.ins(0).valid, inValid)
      if (peek(dut.ins(0).ready) == BigInt(1) && peek(dut.ins(0).valid) == BigInt(1)) {
        poke(dut.ins(0).bits.data, inData(cntIn).toInt)
        println("cntIn")
        println(cntIn.toString)
        println(inData(cntIn).toString)
        cntIn = cntIn + 1
      }
    }
    else {
      poke(dut.ins(0).valid, 0)
    }
    if (peek(dut.outs(0).ready) == BigInt(1) && peek(dut.outs(0).valid) == BigInt(1)) {
      peekedVal0 = peek(dut.outs(0).bits.data)
      peekedVal1 = peek(dut.outs(1).bits.data)
      peekedVal2 = peek(dut.outs(2).bits.data)
      peekedVal3 = peek(dut.outs(3).bits.data)
      assert(peekedVal0 == inData(cntOut*4).toInt)
      assert(peekedVal1 == inData(cntOut*4 + 1).toInt)
      assert(peekedVal2 == inData(cntOut*4 + 2).toInt)
      assert(peekedVal3 == inData(cntOut*4 + 3).toInt)
      /*println(inData(cntOut*4).toString)
      println(inData(cntOut*4 + 1).toString)
      println(inData(cntOut*4 + 2).toString)
      println(inData(cntOut*4 + 3).toString)*/
      cntOut = cntOut + 1
    }
    step(1)
  }
  step (100)
}

class AXI4StreamAdapter1to4Spec extends AnyFlatSpec with Matchers {
  val beatBytes = 4
  implicit val p: Parameters = Parameters.empty

  val testModule = LazyModule(new AXI4StreamAdapter1to4(beatBytes = 4) with AXI4StreamAdapter1to4StandaloneBlock)
  it should f"test AXI4StreamAdapter1to4 Module" in {
    chisel3.iotesters.Driver.execute(Array("verilator"), () => testModule.module) {
      c => new AXI4StreamAdapter1to4Tester(dut = testModule,
                                           beatBytes = 4,
                                           sizeOfInputVector = 8)}  should be (true)
  }
}
