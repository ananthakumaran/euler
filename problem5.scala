/**
 *  @author ananth
 */
 
object problem {
 /* 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
  *  
  *  What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
  */ 
  
  def smallNumber(limit:Int):Int = {
		var result = limit
		(2 until limit).foreach( (x:Int) => result = lcm( x , result))
		result
  }
  
   def lcm(a:Int,b:Int) = a*b/gcd(a,b)
   def gcd(a:Int,b:Int):Int = if (b == 0) a else gcd(b,a%b)
}

assert(problem.smallNumber(10) == 2520)
assert(problem.smallNumber(20) == 232792560)
