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
		 for(i <- 2 until limit ; if(!sieve(i))){
			    (2*i until (limit,i)).foreach(sieve(_) = true )
		 }
		(2 until limit).filter(!sieve(_)).foldLeft(0L)(_+_)
	}
}
assert(Problem.prime(10) == 17)
assert(Problem.prime(2000000) == 142913828922L)