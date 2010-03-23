
/**
  * @author ananthakumaran
  *
  */
import scala.io.Source
object Problem  {
		/*
		 * 22.txt , a 46K text file containing over five-thousand first names, 
		 * begin by sorting it into alphabetical order. Then working out the alphabetical value for each name,
		 * multiply this value by its alphabetical position in the list to obtain a name score.
		 * 
		 * For example, when the list is sorted into alphabetical order,
		 * COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
		 *  So, COLIN would obtain a score of 938  53 = 49714.
		 *
		 * What is the total of all the name scores in the file?
		 **/
		 
	def nameScores(fileName:String)= {
		val worth = Source.fromPath(fileName).mkString.split(",").sortWith(_.compareTo(_)<0).map(
			_.replaceAll("\"", "").map( _.toInt - 64).reduceLeft(_+_) 
		)
		(1 to worth.length).map(x => worth(x-1)*x ).reduceLeft(_+_)
	}
}

assert(Problem.nameScores("22.txt")==871198282)