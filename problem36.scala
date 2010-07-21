/* The decimal number, 585 = 10010010012 (binary), is palindromic in both
 * bases.
 * 
 * Find the sum of all numbers, less than one million, which are
 * palindromic in base 10 and base 2.
 * 
 * (Please note that the palindromic number, in either base, may not
 * include leading zeros.)
 */


object Problem {
  
  def numberPalindrome(limit:Int) = {
    (1 until limit).filter(isPalindrome).filter(x => isPalindrome(toBinary(x))).reduceLeft(_+_)
  }
  
  def toBinary(digit:Int) = {
    var result = ""
    var n = digit
    while(n > 0){
      result += n%2
      n /= 2
    }
    result.reverse
  }
  
  def isPalindrome(digit:Int) = {
    digit.toString.equals(digit.toString.reverse)
  }
  
  def isPalindrome(digit:String) = {
    digit.equals(digit.reverse)
  }
}

assert(Problem.numberPalindrome(1000000) == 872187)
