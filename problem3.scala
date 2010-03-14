/**
 *  @author ananth
 */
object problem {
  /**
   * The prime factors of 13195 are 5, 7, 13 and 29.
   *
   *  What is the largest prime factor of the number 600851475143 ?
   **/

   def largestPrimeFactor(number:Long):Long = {
    var n = number
	var i = 2L
	while(i <= n) 
	{
	  if( n % i == 0)
	  {
	    n = n/i
		if(n == 1)
		{
		   return i
		}
	  }
	  i += 1   
	}
	number
   }
}

assert(problem.largestPrimeFactor(600851475143L) == 6857)


