import scala.io.Source.fromFile
import scala.Option
import scala.Some
import scala.None
import scala.util.Try

implicit class CharArrayExtensions(arr: Array[Char]) {
	def toStr: String = new String(arr)
}

implicit class Pipeable[A](x: A) {
  def pipe[B](callback: A => B): B = callback(x)
  def welp[B](callback: A => B): Option[B] = Try(callback(x)).toOption
}

object Problem1 {
	val data1 = Array(
		"1abc2",
		"pqr3stu8vwx",
		"a1b2c3d4e5f",
		"treb7uchet",
		"elmo"
	) // 142

	val data2 = fromFile("1.input").getLines().toArray

	// We need to find the digits in each string
	// find only the first and last digits
	// turn the pair into an integer
	// then sum
	def solution1(data: Array[String]) = 
		data.flatMap(
			_.filter(_.isDigit)
			.pipe {
				case "" => None
				case str => Some((str.head.toString + str.last).toInt)
			}
		)
		.sum

	val data3 = Array(
		"two1nine",
		"eightwothree",
		"abcone2threexyz",
		"xtwone3four",
		"4nineeightseven2",
		"zoneight234",
		"7pqrstsixteen",
	) // 281

	val lookup = Map(
	  "one" -> "1",
	  "1" -> "1",
	  "two" -> "2",
	  "2" -> "2",
	  "three" -> "3",
	  "3" -> "3",
	  "four" -> "4",
	  "4" -> "4",
	  "five" -> "5",
	  "5" -> "5",
	  "six" -> "6",
	  "6" -> "6",
	  "seven" -> "7",
	  "7" -> "7",
	  "eight" -> "8",
	  "8" -> "8",
	  "nine" -> "9",
	  "9" -> "9"
	)

	val maxlookup = lookup.keys.map(_.length).max

	def solution2(data: Array[String]) =
		data.flatMap(str =>
			// We check every substring of str that's as wide as the longest possible key
			// in the lookup table
			(str + " " * (maxlookup - 1)).sliding(maxlookup)
				// Compare every substr to see if it starts with a key
				// in the lookup table
				.flatMap(s =>
					lookup.keys
						.find(k => s.startsWith(k))
						.flatMap(lookup.get(_))
				)
				.toArray
				.pipe {
					case Array() => None
					case arr => Some((arr.head.toString + arr.last).toInt)
				}
		)
		.sum

	def main(args: Array[String]): Unit = {
		println(runtime.ScalaRunTime.replStringOf(solution1(data1), 15))
		println(runtime.ScalaRunTime.replStringOf(solution1(data2), 15))
		println(runtime.ScalaRunTime.replStringOf(solution2(data3), 15))
		println(runtime.ScalaRunTime.replStringOf(solution2(data2), 15))
	}
}