package fft2rd

case class FFT2RDControlParams (
  rangeFFTSize: Int = 1024,                       // implemented
  dopplerFFTSize: Int =  256,                     // implemented
  addressGenDir: Boolean = true,                  // not sure about this one
  readXYZorXZY: Option[Boolean] = Some(true),     // shall be used only when number of output nodes is equal to 1
  incRegXYZorXZY: Option[Boolean] = Some(false), // not implemented
  pingPong: Boolean = true,                       // implemented
  outputNodes: Int = 4,                           // implemented
  numRxs: Int = 1,                                // implemented
  numTxs: Int = 1                                 // implemented
) {
  final val allowedOutNodes = Seq(1, numRxs, numRxs*numTxs)

  def checkReadXYZorXZYparameter {
    if (outputNodes == 1) {
      require(readXYZorXZY != None, "Parameter readXYZorXZY shall be defined when outputNodes is equal to 1")
      require(incRegXYZorXZY != None, "Parameter incRegXYZorXZY shall be defined when outputNodes is equal to 1" )
    }
  }
  def checkOutputNodes {
    require(allowedOutNodes.contains(outputNodes), s"""Parameter outputNodes must be one of the following: ${allowedOutNodes.mkString(", ")}""")
  }
}



/******* parameter readXYZorXZY

 = 0 -> read dopplerFFTsize samples of one virtual antenna then dopplerFFTsize samples of second and so on until numRxs*numTxs antennas, then loop rangeFFTSize times
 = 1 -> read doopplerFFTSize samples of one virtual antenna, loop it rangeFFTSize times and then switch to next virtual until all numRx*numTxs are read

*******/
