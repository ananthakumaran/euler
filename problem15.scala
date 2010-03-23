/**
 * @author ananthakumaran
 **/


object Problem {
		/*
		 * Starting in the top left corner of a 2*2 grid, there are 6 routes (without backtracking) to the bottom right corner.
         * 
		 * see 15.gif
		 * How many routes are there through a 20*20 grid?
		 *
		 **/
	
	
	var limit = 0
	def routes(x:Int) = {
	// Brute force attack taking too much time for 20 * 20
	//	limit = x
    // maxRoute(0,0)
		factorial(2*x)/factorial(x).pow(2)
	}
	
	def factorial(x:Int) = (1 to x).foldLeft(BigInt(1))(_*_) 
	
	def maxRoute(x:Int,y:Int):Int = {
		if(x == limit || y == limit)
			 1			 
	    else if(x > limit || y > limit)
			 0
		else
		  maxRoute(x+1,y) + maxRoute(x,y+1)	
	}
} 
 
assert(Problem.routes(2) == 6) 
assert(Problem.routes(20) == 137846528820) 
 