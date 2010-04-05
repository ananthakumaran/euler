/**
 * @author ananthakumaran
 */
object Problem {
/**
 *	Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 *  If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and each of a and b are called amicable numbers.
 *
 *  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
 *  therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 **/	
	
	
	def sumOfAmicableNumbers(limit:Int) = {
		val div = 0 :: 0 :: (2 until limit).map(sumOfProperDivisors).toList
		(2 until limit).filter(n => (div(n)<limit && n == div(div(n))) && n != div(n)).reduceLeft(_+_)
	}
	
	def sumOfProperDivisors(n:Int) = (1 to (n/2)+1).filter(n%_ == 0).reduceLeft(_+_)
}

assert(Problem.sumOfAmicableNumbers(10000) == 31626)

