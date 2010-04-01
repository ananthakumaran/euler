/**
 * @author ananthakumaran
 **/

object Problem {

			/** A permutation is an ordered arrangement of objects. For example, 3124 is one possible
			 * permutation of the digits 1, 2, 3 a * nd 4. If all of the permutations are listed numerically
			 * or alphabetically, we callit lexicographic order.The lexicograhic
			 * permutations of 0, 1 and 2 are:
			 *
			 * 012   021   102   120   201   210
			 *
			 * What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
			 *
			 **/

	def perm = {		
		var test = Array(0, 1, 2, 3, 4, 5, 6, 7, 8 , 9)
		(1 until 1000000).foreach((x:Int) => { 
			nextPermutation(test) 
		} )	
		test.toList
	}
	def nextPermutation(cur : Array[Int]) = {
		val length = cur.length
		val opt = (1 to length-1).reverse.find(i => (cur(i) > cur(i-1)))
		
		if(opt.isEmpty)
			false
		else
		{
			var pivot = opt.get - 1
			var i = pivot + 2
			var notFound = true
			while(i < length && notFound)
			{
				if(cur(i)<cur(pivot))
					notFound = false	
				else
					i+=1
			}
			i -= 1
			
			val temp = cur(pivot)
			cur(pivot) = cur(i)
			cur(i) = temp
			
			cur.takeRight(length - (pivot + 1)).reverse.foreach(x => {
				pivot += 1
				cur(pivot) = x
			})
			true
		}
	}
}

assert(Problem.perm == List(2,7,8,3,9,1,5,4,6,0))