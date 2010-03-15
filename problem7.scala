/**
 * @author ananth
 */
 
object Problem {

    /* 
	 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
	 *
	 * What is the 10001st prime number?
	 **/
	
	
	def prime(limit:Int) = {
		var count = 1
		var i:BigInt = 1
		while(count < limit){
			i+=2
			if( i.isProbablePrime(5))
			  count += 1
		}
		i
	}
}

assert(Problem.prime(6) == 13)
assert(Problem.prime(10001) == 104743)




