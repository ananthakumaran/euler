object problem {
   /** If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
     * Find the sum of all the multiples of 3 or 5 below 1000.
	 **/
	def main(args: Array[String]) {
	 assert(sumOfMultiples(10) == 23)
	 print(sumOfMultiples(1000))
   }
  
  def sumOfMultiples(limit:Int):Int = {
     var result = 0
	    var i = 0
	    while(i<limit)
		{
		    if(i%3 == 0 || i%5 == 0)
			{
			result += i
			}
			i+=1
		}
	 result
  }
}
