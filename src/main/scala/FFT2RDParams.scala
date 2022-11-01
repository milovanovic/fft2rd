package fft2rd

import chisel3._
import dsptools.numbers._
import freechips.rocketchip.diplomacy._
import fft._
import zeropadder._

case class FFT2RDParams[T <: Data : Real: BinaryRepresentation] (
  fft2ControlParams     : FFT2RDControlParams  = FFT2RDControlParams (
                          rangeFFTSize = 1024 ,
                          dopplerFFTSize =  256,
                          addressGenDir = true,
                          outputNodes = 12,
                          numRxs = 4,
                          numTxs = 3
                        ),
  zeroPadderRangeParams  : Option[ZeroPadderParams[T]] = None, // if parameters are None then do not add zerropadder module
  zeroPadderDopplerParams: Option[ZeroPadderParams[T]] = None,
  rangeFFTParams         : FFTParams[T],
  dopplerFFTParams       : FFTParams[T],
  fft2ControlAddress     : AddressSet,
  rangeFFTAddress        : AddressSet,
  dopplerFFTAddress      : AddressSet,
  zeroPadderRangeAddress : Option[AddressSet] = None,
  zeroPadderDopplerAddress: Option[AddressSet] = None
)
