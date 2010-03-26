 /** 
  * @author ananthakumaran
  **/

object Problem {

        /**
	 * The Fibonacci sequence is defined by the recurrence relation:
         * Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
       	 * Hence the first 12 terms will be:
 	 *
	 * F1 = 1
	 * F2 = 1
	 * F3 = 2
         * F4 = 3
	 * F5 = 5
	 * F6 = 8
         * F7 = 13
	 * F8 = 21
         * F9 = 34
	 * F10 = 55
	 * F11 = 89
         * F12 = 144
         * The 12th term, F12, is the first term to contain three digits.
      	 *
	 * What is the first term in the Fibonacci sequence to contain 1000 digits?
         **/
    
   var f1 = BigInt(1)
   var f2 = BigInt(1)

   def next():BigInt = {
      val result = f1 + f2
      f1 = f2
      f2 = result
      f2
   }

    def term(limit:Int):Int = {
    	var count = 3
        while(next.toString.length < limit)
        {
       		count += 1
        }
    	count
   }
}

assert(Problem.term(1000)==4782)