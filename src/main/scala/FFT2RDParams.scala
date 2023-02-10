package fft2rd

import chisel3._
import dsptools.numbers._
import freechips.rocketchip.diplomacy._
import fft._
import zeropadder._
import windowing._

case class FFT2RDParams[T <: Data : Real: BinaryRepresentation] (
  fft2ControlParams     : FFT2RDControlParams  = FFT2RDControlParams (
                          rangeFFTSize = 1024 ,
                          dopplerFFTSize =  256,
                          addressGenDir = true,
                          outputNodes = 12,
                          numRxs = 4,
                          numTxs = 3
                        ),
  zeroPadderRangeParams   : Option[ZeroPadderParams[T]] = None,
  zeroPadderDopplerParams : Option[ZeroPadderParams[T]] = None,
  winRangeParams          : Option[WindowingParams[T]] = None,
  winDopplerParams        : Option[WindowingParams[T]] = None,
  rangeFFTParams          : FFTParams[T],
  dopplerFFTParams        : FFTParams[T],
  fft2ControlAddress      : AddressSet,
  rangeFFTAddress         : AddressSet,
  dopplerFFTAddress       : AddressSet,
  zeroPadderRangeAddress  : Option[AddressSet] = None,
  zeroPadderDopplerAddress: Option[AddressSet] = None,
  winRangeAddress         : Option[AddressSet] = None, // csr
  winDopplerAddress       : Option[AddressSet] = None, // csr
  winRangeRAMAddress      : Option[AddressSet] = None,
  winDopplerRAMAddress    : Option[AddressSet] = None
)
