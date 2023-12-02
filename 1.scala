import scala.io.Source.fromFile
import scala.util.chaining.scalaUtilChainingOps

object Problem1 {
	val data1 = List(
		"1abc2",
		"pqr3stu8vwx",
		"a1b2c3d4e5f",
		"treb7uchet",
		"elmo"
	) // 142

	val data2 = fromFile("1.input").getLines().toList

	// We need to find the digits in each string
	// find only the first and last digits
	// turn the pair into an integer
	// then sum
	def solution1(data: List[String]) = 
		data.flatMap(
			_.filter(_.isDigit)
			match {
				case "" => None
				case str => Some(s"${str.head}${str.last}".toInt)
			}
		)
		.sum

	val data3 = List(
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
	  "two" -> "2",
	  "three" -> "3",
	  "four" -> "4",
	  "five" -> "5",
	  "six" -> "6",
	  "seven" -> "7",
	  "eight" -> "8",
	  "nine" -> "9",
	).flatMap((key, value) => Seq(key -> value, value -> value));

	val maxlookup = lookup.keys.map(_.length).max

	def solution2(data: List[String]) =
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
				.toList
				match {
					case List() => None
					case arr => Some(s"${str.head}${str.last}".toInt)
				}
		)
		.sum

	def main(args: Array[String]): Unit = {
		println("part1-sample: " ++ runtime.ScalaRunTime.replStringOf(solution1(data1), 15))
		println("part1-answer: " ++ runtime.ScalaRunTime.replStringOf(solution1(data2), 15))
		println("part2-sample: " ++ runtime.ScalaRunTime.replStringOf(solution2(data3), 15))
		println("part2-answer: " ++ runtime.ScalaRunTime.replStringOf(solution2(data2), 15))
	}
}