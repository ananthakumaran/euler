/**
  * @author ananthakumaran
  * 
  **/
  
object Problem {
		/**
		  *	2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
		  *	What is the sum of the digits of the number 2^1000?
		  *
		  **/
		  
		  def sumOfDigitsOfPowers(n:BigInt,p:Int) = {
				sumOfDigits(n.pow(p))
		  }
		  
		  def sumOfDigits(number:BigInt) = {
				var n = number
				var result = BigInt(0)
				while(n>0){
					result += n%10
					n = n/10
				}
				result
		  }
}

assert(Problem.sumOfDigitsOfPowers(2,15) == 26)
assert(Problem.sumOfDigitsOfPowers(2,1000) == 1366)
