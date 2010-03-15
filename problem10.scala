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
		 val sieve = new Array[Boolean](limit)
		 for(i <- 2 until limit ; if(!sieve(i) && isPrime(i))){
			    (2*i until (limit,i)).foreach(sieve(_) = true )
		 }
		(2 until limit).filter(!sieve(_)).foldLeft(0L)(_+_)
	}
	
	def isPrime(n:Long):Boolean = {
	    if(n < 2) return false
        if(n == 2 || n == 3) return true
        if(n%2 == 0 || n%3 == 0) return false
        val sqrtN = Math.sqrt(n)+1
        var i = 6L
		while( i <= sqrtN )
		{
			if(n%(i-1) == 0 || n%(i+1) == 0) 
				return false
			i += 6	
		}
		true	
	}
}
assert(Problem.prime(10) == 17)
assert(Problem.prime(2000000) == 142913828922L)