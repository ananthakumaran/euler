/**
  * @author ananth
  */
object problem {
   /** If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
     * Find the sum of all the multiples of 3 or 5 below 1000.
	 **/
  
  def sumOfMultiples(limit:Int):Int = {
     var result = 0
	 (0 until limit).filter( i => i%3==0 || i%5==0 ).foreach( result += _)
	 result
  }
}


assert( problem.sumOfMultiples(10) == 23)
assert( problem.sumOfMultiples(1000) == 233168)
