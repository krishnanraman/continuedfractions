/**
*  Periodic Continued Fractions in Scala
*  @author: Krishnan Raman (kraman@twitter.com)
*
*  Reference: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/cfINTRO.htm
*  A continued fraction looks like
*  constant + (num1/(denom1 + num2/(denom2 + num3/(denom3 + ...
*  So-called list notation : [constant, denom1,denom2,denom3,denom4,...] , num1 = num2 = num3 = ... = 1
*  For transcendentals, list notation isn't as useful as the very flexible (x:Int=>Int) function notation for num & denom
*/
object CF extends App {

  def apply(name:String, list: Seq[Int]) = new CF(name, list)

  // A dozen continued fractions below. Enjoy!
  def goldenratio = CF("Golden Ratio",   Seq(1,1))
  def sqrt2 = CF("Square root of 2",     Seq(1,2))
  def sqrt3 = CF("Square root of 3",     Seq(1,1,2))
  def sqrt5 = CF("Square root of 5",     Seq(2,4))
  def sqrt7 = CF("Square root of 7",     Seq(2,1,1,1,4))
  def sqrt13 = CF("Square root of 13",     Seq(3,1,1,1,1,6))
  def exp = CF("e aka Natural log base, Euler expansion:", 2, (x: Int) => if (x == 1) 1 else (x-1), (x: Int) => x)
  def expminus1 = CF("e-1 , Euler expansion", 1, (x: Int) => x+1, (x: Int) => x+1)
  def expnthroot(n: Int) = CF("e ^ (1/" + n + ")", 1, (x: Int) => 1, (x: Int) => if ((x-1) % 3 == 0) ((2*(x-1)/3)+1) * n - 1 else 1)
  def pi = CF("Pi, by L.J.Lange (AMM 1999)", 3, (x: Int) => (2*x-1)*(2*x-1), (x: Int) => 6, 100)
  def fourbypi = CF("4/Pi, by William Brouncker (1655)", 1, (x: Int) => (2*x-1)*(2*x-1), (x: Int) => 2, 25)
  def rabbitconstant = CF("Rabbit Constant", 0, (x: Int) => 1, (x: Int) => math.pow(2, fibs(x-1)).toInt, 5)

  lazy val fibs: Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

  List (goldenratio, sqrt2, sqrt3, sqrt5, sqrt7, sqrt13, exp, expminus1, expnthroot(3), pi, fourbypi, rabbitconstant) foreach println

}

case class CF(name:String, constant: Int, numerator: (Int=>Int), denominator: (Int=>Int), level:Int = 30) {

  def this(name:String, list: Seq[Int]) = {
    this(name, list.head, (x:Int) => 1,
      (x:Int) =>  if (list.size == 2) list(1)
                  else if(( x% list.tail.size) == 0) list.last
                  else list.tail((x % list.tail.size) - 1))
  }

  def eval(level: Int): (BigInt, BigInt) = {

    def nk(k: Int): (BigInt, BigInt) = (numerator(k), denominator(k) + numerator(k + 1))

    def nkrecurse(level: Int): (BigInt, BigInt) = {
      (level to 1 by -1).foldLeft(nk(level + 1)) { (init, k) => {
          val (num, denom) = init
          (numerator(k) * denom, (denominator(k) * denom) + num)
        }
      }
    }

    val (num, denom) = nkrecurse(level)
    (denom * constant + num, denom)
  }

  def evalS(level: Int): String = {
    val (num, denom) = eval(level)
    num + "/" + denom
  }

  def evalF(level: Int): Double = {
    val (num, denom) = eval(level)
    num.doubleValue / denom.doubleValue
  }

  override def toString = name + " = " + evalF(level) +  " = " + evalS(level)
}
