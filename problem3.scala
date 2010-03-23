/**
 *  @author ananth
 */
object Problem {
  /**
   * The prime factors of 13195 are 5, 7, 13 and 29.
   *
   *  What is the largest prime factor of the number 600851475143 ?
   **/

   def largestPrimeFactor(number:Long):Long = {
    var n = number
	var i = 2L
	while(n != 1) 
	{ 
	  if( n % i == 0)
	    n = n/i
	  else 
		i += 1   
	}
	i
   }
}

assert(Problem.largestPrimeFactor(600851475143L) == 6857)
assert(Problem.largestPrimeFactor(8) == 2)


