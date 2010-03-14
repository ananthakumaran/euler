/**
 *  @author ananth
 */
object problem { 
  
  /**
   * The sum of the squares of the first ten natural numbers is,
   *
   * 12 + 22 + ... + 102 = 385
   * The square of the sum of the first ten natural numbers is,
   *
   * (1 + 2 + ... + 10)2 = 552 = 3025
   * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.
   *
   * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
   **/
   
  def diff(limit:Int) = Math.pow(sumOfNumber(limit),2) - sumOfSquares(limit)
  def sumOfSquares(limit:Int) = (1 to limit).foldLeft(0)((a,b) =>  a + b*b )
  def sumOfNumber(limit:Int) = (limit * (limit + 1))/2
}


assert(problem.diff(10) == 2640)
assert(problem.diff(100) == 25164150)