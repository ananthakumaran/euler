/**
 * @author ananth
 */
object problem {

    /** 
	  * A palindromic number reads the same both ways.
      * The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.
      * Find the largest palindrome made from the product of two 3-digit numbers.
	  **/
 
   
  def largestPalindrome(limit:Int):Int = {
		var max = 0
		(100 until 999).foreach( i => {
			 (100 until 999).filter(j => 
				(isPalindrome(i*j + ""))).foreach( k => 
				    max = Math.max(i*k,max)
				)
		})
		max
   }
	  
	  
  def isPalindrome(str:String) = str == str.reverse.toString
  // NOTE no need for toString in Scala 2.8
}

assert(problem.largestPalindrome(999) == 906609)