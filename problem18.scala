/**
 * @author ananthakumaran
 */

/*
 * By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
 *
 *	3
 *	7 4
 *	2 4 6
 *	8 5 9 3
 *
 *	That is, 3 + 7 + 4 + 9 = 23.
 *
 * Find the maximum total from top to bottom of the triangle below:
 *
 *  see 18.txt
 * 
 * NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route.
 * However, Problem 67, is the same challenge with a triangle containing one-hundred rows; 
 * it cannot be solved by brute force, and requires a clever method! ;o)
 **/

/**
 *  TODO cleanup the build method
 *
 */
 
import scala.io.Source
object Problem {

	def build(fileName:String) = {
		val tri = Source.fromPath(fileName).mkString.lines.toList	
		var	tree = Array.fill(tri.length,tri.length)(0)
		for(i <- 0 until tri.length){
			val line = tri(i).split(' ')	
			var temp = new Array[Int](tri.length)
			(0 to i).foreach(j => temp(j) = line(j).toInt)
			tree(i) = temp 
		}
		tree
	}

	def maxWeight(fileName:String):Int = {
		var tree = build(fileName)
		for(i <- 1 until tree.length) {
			var temp = new Array[Int](tree.length)
			(0 to i).foreach(j => temp(j) = tree(i)(j) + Math.max(tree(i-1)(Math.max(j-1,0)) , tree(i-1)(j)))
			tree(i) = temp 
		}
		tree(tree.length-1).toList.reduceLeft(Math.max(_,_))
	}
} 

assert(Problem.maxWeight("18.txt") == 1074)