package utils

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

import chisel3.iotesters.Driver
import chisel3.iotesters.PeekPokeTester
//import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import scala.math.pow


class HeaderInserterTester(
  dut: AXI4HeaderInserterBlock with HeaderInserterPins,
  beatBytes: Int,
  mode1024: Boolean = true
)  extends PeekPokeTester(dut.module) with AXI4StreamModel with AXI4MasterModel {

  def memAXI: AXI4Bundle = dut.ioMem.get
  
  //val mode1024: Boolean = true
  
  val length = if (mode1024) 257*12*32 else 193*2*256
  //val length = 257*12*32
  //val length = 193*2*256
  var headerVal = 0
  val expectedVal = new Array[Int](length)
  
  var ii = 0
  while (ii < length) {
    var firstCounter = if (mode1024) (ii / (257*32)) % 12 else (ii / 193) % 2 
    var secondCounter = if (mode1024) (ii / 257) % 32  else (ii / (193*2)) % 256
    headerVal = pow(2, 16).toInt*165 + pow(2, 8).toInt*firstCounter + secondCounter
    
    if (mode1024) {
      if ((ii % 257) != 0) expectedVal(ii) = ii
      if ((ii % 257) == 0) expectedVal(ii) = headerVal
    } else {
      if ((ii % 193) != 0) expectedVal(ii) = ii
      if ((ii % 193) == 0) expectedVal(ii) = headerVal
    }
    
    ii += 1
  }
  

  poke(dut.out.ready, 0)
  poke(dut.in.valid, 0)
  poke(dut.in.bits.data, 0)
  poke(dut.in.bits.last, 0)
  
  step(5)
  
  val rand = new Random
  //val rand1 = new Random
  
  var i = 0
  var peekedVal: BigInt = 0
  var peekedValLast: BigInt = 0
  
  //poke(dut.out.ready, 1)
  poke(dut.out.ready, rand.nextInt(2))
  step(1)
  
  //while(i < 1024*32*12*2) {
  //while(i < 256*32*12) {
  while(i < length) {
    //poke(dut.in.valid, 1)
    poke(dut.out.ready, rand.nextInt(2))
    poke(dut.in.valid, rand.nextInt(2))
    poke(dut.in.bits.data, i)
    //if((peek(dut.in.ready) == 1) && (peek(dut.in.valid) == 1)){
    if((peek(dut.out.ready) == 1) && (peek(dut.out.valid) == 1)){
      
      peekedVal = peek(dut.out.bits.data)
      assert(peekedVal.toInt == expectedVal(i))
      
      peekedValLast = peek(dut.out.bits.last)
      if (mode1024) {
        if(((i % 257) == 256)) {
          assert(peekedValLast.toInt == 1)
        } else {
          assert(peekedValLast.toInt == 0)
        }
      } else {
        if(((i % 193) == 192)) {
          assert(peekedValLast.toInt == 1)
        } else {
          assert(peekedValLast.toInt == 0)
        }
      }  
      
      i += 1
    }
    step(1)
  }
  
  i = 0
  while(i < length) {
    //poke(dut.in.valid, 1)
    poke(dut.out.ready, rand.nextInt(2))
    poke(dut.in.valid, rand.nextInt(2))
    poke(dut.in.bits.data, i)
    //if((peek(dut.in.ready) == 1) && (peek(dut.in.valid) == 1)){
    if((peek(dut.out.ready) == 1) && (peek(dut.out.valid) == 1)){
      
      peekedVal = peek(dut.out.bits.data)
      assert(peekedVal.toInt == expectedVal(i))
      
      peekedValLast = peek(dut.out.bits.last)
      if (mode1024) {
        if(((i % 257) == 256)) {
          assert(peekedValLast.toInt == 1)
        } else {
          assert(peekedValLast.toInt == 0)
        }
      } else {
        if(((i % 193) == 192)) {
          assert(peekedValLast.toInt == 1)
        } else {
          assert(peekedValLast.toInt == 0)
        }
      }  
      
      i += 1
    }
    step(1)
  }
  
  
  poke(dut.in.valid, 0)
  
  step(20)

}


class HeaderInserterSpec extends AnyFlatSpec with Matchers {
  
  implicit val p: Parameters = Parameters.empty
  val beatBytes = 4
  val addresses = AddressSet(0x50000000 , 0xFF)
  val mode1024 = false
  
  val testModule = LazyModule(new AXI4HeaderInserterBlock(mode1024, addresses, beatBytes) with HeaderInserterPins)
  it should "Test Header Inserter" in {
    chisel3.iotesters.Driver.execute(Array("verilator"), () => testModule.module) {
      c => new HeaderInserterTester(dut = testModule, beatBytes = beatBytes, mode1024 = mode1024) //, params = paramsFFT2D)
    } should be (true)
  }
  
}
