/*
 * In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
 *
 * 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
 * It is possible to make £2 in the following way:
 *
 * 1£1 + 150p + 220p + 15p + 12p + 31p
 * How many different ways can £2 be made using any number of coins?
 * 
 */

object Problem {
 
  def countChange(amount:Int,n:Int):Int = {
    if(amount == 0)
      1
    else if(amount < 0 || n == 0)
      0
    else
      countChange(amount, n-1) + countChange(amount - value(n) , n) 
  }
  
  def value(x:Int) = x match {
    case 1 => 1
    case 2 => 2
    case 3 => 5
    case 4 => 10
    case 5 => 20
    case 6 => 50
    case 7 => 100
    case 8 => 200
  }
}

assert(Problem.countChange(200,8) == 73682)
