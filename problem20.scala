/**
 * @author ananth
 * 
 */

object Problem {
	/**
	 *  n! means n  (n  1)  ...  3  2  1
	 *  Find the sum of the digits in the number 100!
	 */
	 
	 def sumOfDigitsOfFactorial(n:Int) = sumOfDigits(factorial(n))
	 
	 def sumOfDigits(number:BigInt) = {
		var n = number
		var result = BigInt(0)
		while(n > 0) {
		  result += n%10
		  n = n/10
		}
		result
	 }
	 def factorial(n:Int):BigInt = ( 1 to n).foldLeft(BigInt(1))(_*_)
}


assert(Problem.sumOfDigitsOfFactorial(3) == 6)
assert(Problem.sumOfDigitsOfFactorial(100) == 648)


