/**
  * @author ananth
  *
  */

object Problem {
	/**
	 * If the numbers 1 to 5 are written out in words: one, two, three, four, five,
	 * then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
	 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
     * how many letters would be used?
	 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
	 * contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
	 * The use of "and" when writing out numbers is in compliance with British usage.
	 */

	
	def countOfLettersOfWordsOfNumbers(limit:Int):Int = {
		var result = 0
		(1 to limit).foreach(x => result += (toWord(x)).length )
		result
	}
	 
	 
	 def toWord(x:Int):String = {
		if(x<20)
		{
			single(x)
		}
		else if(x < 100)
		{
			double(x/10) + toWord(x%10)
		}
		else if(x < 1000){
		   triple(x/100)+ (if (x%100 == 0) "" else "and") + toWord(x%100)
		}
		else {
			"onethousand"
		}
	 }
	 
	 
	 def single(x:Int):String = x match {
		case 0 => ""
		case 1 => "one"
		case 2 => "two"
		case 3 => "three"
		case 4 => "four"
		case 5 => "five"
		case 6 => "six"
		case 7 => "seven"
		case 8 => "eight"
		case 9 => "nine"
		case _ => teen(x)
	 }
	 def teen(x:Int):String = x match {
		case 10 => double(1)
		case 11 => "eleven"
		case 12 => "twelve"
		case 13 => "thirteen"
		case 14 => "fourteen"
		case 15 => "fifteen"
		case 16 => "sixteen"
		case 17 => "seventeen"
		case 18 => "eighteen"
		case 19 => "nineteen"
	 }
	 def double(x:Int):String = x match {
		case 1 => "ten"
		case 2 => "twenty"
		case 3 => "thirty"
		case 4 => "forty"
		case 5 => "fifty"
		case 6 => "sixty"
		case 7 => "seventy"
		case 8 => "eighty"
		case 9 => "ninety"
	 }
	 def triple(x:Int):String = x match {
		case _ => single(x) + "hundred"
	 }
}  
  
assert(Problem.countOfLettersOfWordsOfNumbers(5)==19)
assert(Problem.countOfLettersOfWordsOfNumbers(1000) == 21124)

