/**
 * @author ananth
 */
 
object Problem {

    /* 
	 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
	 *
	 * Find the sum of all the primes below two million.
	 **/
	
	
	def prime(limit:Int) = {
		var result = 0L
		for(i <- 2 to limit ; if(isPrime(i))){
			  result += i
		}
		result
	}
	
	def isPrime(n:Int):Boolean = {
		if(n<=1) return false
		if(n==2) return true
		if(n%2 ==0) return false
		var i = 3
		while( i <= Math.sqrt(n) +1 )
		{
			if( n%i == 0 )
				return false
			i += 2	
		}
		true
	}
}

assert(Problem.prime(10) == 17)
assert(Problem.prime(2000000) == 142913828922)