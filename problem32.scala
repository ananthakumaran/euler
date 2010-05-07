/*
 *We shall say that an n-digit number is pandigital if it makes use of all the digits 1
 * to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
 *
 *The product 7254 is unusual, as the identity, 39 * 186 = 7254,
 * containing multiplicand, multiplier, and product is 1 through 9 pandigital.
 *
 *Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
 *
 */


object Problem {

  def pandigital(digits:Int) = {
    val permutations = permutate((1 to digits).toList)
    var sum = Set[Long]() 
    permutations.foreach(lat => {
      (1 to lat.length - 2).foreach(i => {
	(i+1 to lat.length - 1).foreach(j => {
	  val multiplicand = lat.take(i).mkString.toLong
	  val multiplier = lat.slice(i,j).mkString.toLong
	  val product = lat.takeRight(lat.length - j).mkString.toLong
	  
	  if(multiplicand * multiplier == product)
	    sum += product
	})
      })
    }) 
    sum.reduceLeft(_+_)
  }
  
  def  permutate(list:List[Any]):List[List[Any]] = list.size match {

    // For example, to get the permutations of {1,2,3}:
    //        Find solution to permutations of {1, 2}
    //              Find solution to permutations of {1}
    //              This is just {1}
    //      Insert 2 into {1}, giving: {2 1} and {1 2}
    // Insert 3 into {2 1} and {1 2} giving: {3 2 1} {2 3 1} {2 1 3} {3 1 2} {1 3 2} {1 2 3}

    case 1 => List(list)
    case _ => insert(permutate(list.tail),list.head) 
  }
  

  def insert(lat:List[List[Any]],item:Any) = {
    var result:List[List[Any]] = List()
    lat.foreach(x => {
      x.indices.foreach(i => {
	result =  (x.take(i) ::: List(item) ::: x.takeRight(x.length - i)) :: result 
      })
      result =  (x ::: List(item)) :: result
    })
    result
  }	

}

assert(Problem.pandigital(9) == 45228)
