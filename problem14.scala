/**
 * @author ananthakumaran
 */

import scala.collection.mutable 
object Problem {
	/**
	 *
	 *The following iterative sequence is defined for the set of positive integers:
	 *
	 * n  n/2 (n is even)
	 * n  3n + 1 (n is odd)
	 *
	 * Using the rule above and starting with 13, we generate the following sequence:
	 *
	 * 13  40  20  10  5  16  8  4  2  1
	 * It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
	 * Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
	 * 
	 * Which starting number, under one million, produces the longest chain?
	 *
	 * NOTE: Once the chain starts the terms are allowed to go above one million.
	 **/
	 
	var chainCache = Map[Long,Int]()
	 
	def chainLength(start:Long) = {
		var n = start
		var length = 1
		var hit = false
		while(n != 1 && !hit) {
			n = (if (n%2==0) n/2 else 3*n+1)
			if(chainCache.contains(n))
			{
				length += chainCache(n)
				hit = true
			}
			else
			{
				length += 1
			}			
		}
		chainCache += (start -> length)
		length
	}
	
	def longestChain(limit:Int) = {
		 var longest = (0,0)
		(1 to limit).foreach( i => {
						val length = chainLength(i)
						if(longest._2 <= length)
							longest = (i,length)
					})
		longest._1
	}
} 

assert(Problem.longestChain(1000000)==837799)
 
