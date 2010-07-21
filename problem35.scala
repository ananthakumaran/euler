/*
 * The number, 197, is called a circular prime because all rotations of
 * the digits: 197, 971, and 719, are themselves prime.
 *
 * There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31,
 * 37, 71, 73, 79, and 97.
 * How many circular primes are there below one million?
 */

object Problem {
  def circularPrimes(limit:Int) = {
    (1 to limit).filter(isPrime).filter(x => {
      rotate(x).forall(isPrime)
    }).size
  }
  
  def rotate(n:Int) = {
    val size = n.toString.length
    val d = Math.pow(10,size - 1)
    var result = List(n)
    (1 until size).foreach(x => {
      result ::= ((result.head % d * 10) + (result.head / d)).toInt
    })
    result
  }
  
  def isPrime(n:Int):Boolean = {
    if(n < 2)
      return false
    else if (n == 2)
      return true
    else if(n % 2 == 0)
      return false
    else {
      var i = 3
      while(i <= Math.sqrt(n)+1) {
	if(n%i == 0)
	  return false
	i += 2
      }
    }
    return true
  }
}

assert(Problem.circularPrimes(100) == 13)
assert(Problem.circularPrimes(1000000) == 55)
