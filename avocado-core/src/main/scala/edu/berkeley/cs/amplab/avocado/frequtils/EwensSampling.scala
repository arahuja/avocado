package edu.berkeley.cs.amplab.avocado.frequtils

object EwensSampling {

  implicit def factorial(n:Int) = new { def ! = ((1 to n) :\ 1) ( _ * _ ) }

  def pOfAlleleDistribution(  alleleCounts : Seq[Int], theta : Double) : Double =
  {
    val n = alleleCounts.zipWithIndex.map( _._1 * _._2 ).reduce( _ + _)
    val scalaDenom = (0 until n).map( theta + _ ).reduce( _ * _ )
    val scalar = n! / scalaDenom

    val p = alleleCounts.zipWithIndex.map( pow ( theta, _._1 ) / (pow(_._2, _._1) * _._1!))

    p * scalar
  }

}
