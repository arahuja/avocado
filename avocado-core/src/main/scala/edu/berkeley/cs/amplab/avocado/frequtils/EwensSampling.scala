package edu.berkeley.cs.amplab.avocado.frequtils

object EwensSampling {


  def factorial(n: Long, t : Double = 1): Double = if (n == 0) t else factorial(n-1, t*n)
  //implicit def factorial(n:Int) : Long = new { def ! = ((1 to n.toLong) :\ 1.toLong) ( _ * _ ) }
  //def factorial(n : Int) = (1 to n).reduce( _ * _ )
  def pOfBucketSize( bin : Int, binSize : Int, theta : Double) : Double =
  {
    val p = math.pow ( theta, binSize )  / (math.pow(bin, binSize) * factorial(binSize) )
    p
  }

  def pOfBinDistribution(  binCounts : Seq[Int], theta : Double) : Double =
  {
    // n = a_1 + 2*a_2 + 3* a_3 ...
    val n = binCounts.zipWithIndex.map( b => b._1 * (b._2 + 1) ).reduce( _ + _)
    val scalaDenom = (0 until n).map( theta + _ ).reduce( _ * _ )
    val scalar = factorial(n) / scalaDenom.toDouble

    val p = scalar * binCounts.zipWithIndex.map( b => pOfBucketSize( b._2 + 1, b._1, theta)).reduce( _ * _)

    p

  }

}
