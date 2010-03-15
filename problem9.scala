/**
 * @author ananth
 */
 
object Problem {
	/**
	 * A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
     *
	 * a2 + b2 = c2
	 * For example, 32 + 42 = 9 + 16 = 25 = 52.
	 *	
	 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
	 * Find the product abc.
	 **/
	
    def triplet()= {
	  var result = 0
	 for(i <- 1 to 999 ){
		for(j <- 1 to 999){
			for(k <- 1 to 999 ; if((i+j+k == 1000) && (i*i + j*j == k*k) )){
				result = i*j*k
			}
		}
	 }
	 result
	}
} 

assert(Problem.triplet() == 31875000)