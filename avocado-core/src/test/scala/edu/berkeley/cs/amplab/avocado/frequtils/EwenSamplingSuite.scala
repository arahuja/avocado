package edu.berkeley.cs.amplab.avocado.frequtils

import org.scalatest.FunSuite


class EwenSamplingSuite extends FunSuite {

    def assertFP(a: Double, b: Double) {
      assert((a * 0.99 < b && a * 1.01 > b) ||
        math.abs(a - b) < 1e6)
    }

    test("Ewens Sampling") {

      val p = EwensSampling.pOfBinDistribution( Seq( 10, 10 ), 1)
      assert(p > 0)

    }

}
