package day03
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

implicit class Dumpable[A](x: A) {
	def DUMP[B] = runtime.ScalaRunTime.replStringOf(x, 1000)
}

val data1 = Source.fromString(
	"""467..114..
	  |...*......
	  |..35..633.
	  |......#...
	  |617*......
	  |.....+.58.
	  |..592.....
	  |......755.
	  |...$.*....
	  |.664.598..
	  |""".stripMargin
).getLines().toList

val data2 = Source.fromFile("3.input").getLines().toList

case class PartNumber(id: Int, row: Int, col: Int) {
	override
	def toString = s"PartNumber($id)"
}
case class Part(symbol: String, row: Int, col: Int)

class Row(val columns: Vector[PartNumber | String]) extends Iterable[PartNumber | String] {
	def iterator: Iterator[PartNumber | String] = columns.iterator
	override
	def stringPrefix = "Row"

	def xxx(col: Int): Iterable[PartNumber | String] = columns.lift(col - 1) ++ columns.lift(col) ++ columns.lift(col + 1)
	def x_x(col: Int): Iterable[PartNumber | String] = columns.lift(col - 1) ++ columns.lift(col + 1)
}

class Schema(val rows: Vector[Row]) extends Iterable[Row] {
	def iterator: Iterator[Row] = rows.iterator
	override
	def stringPrefix = "Schema"

	def findAdjacentPartNumbers(part: Part): List[PartNumber] =
		(
			rows.lift(part.row - 1).map(_.xxx(part.col).collect { case x: PartNumber => x }) ++
			rows.lift(part.row    ).map(_.x_x(part.col).collect { case x: PartNumber => x }) ++
			rows.lift(part.row + 1).map(_.xxx(part.col).collect { case x: PartNumber => x })
		).flatten.toList.distinct


	def parts =
		// Visit every row
		rows.map(
			// Visit every column and pick out only the part symbols
			_.columns.zipWithIndex
				.collect { case (s: String, i) if s != "." => (s, i) }
		).zipWithIndex
		.collect {
			case (v, row) if v.length != 0 => 
				v.map { case (symbol, col) => Part(symbol, row, col) }
		}.toList.flatten
}

object Schema {
	private val tokenizer = raw"\d+|\D".r

	def fromLines(data: Iterable[String]) = 
		data.zipWithIndex
			.map { case (dataLine, row) =>
				tokenizer.findAllIn(dataLine).zipWithIndex.flatMap {
					case (token, col) if token.head.isDigit => List.fill(token.length)(PartNumber(token.toInt, row, col))
					case (token, _) => List(token)
				}
				.pipe(x => new Row(x.toVector))
			}
			.pipe(x => new Schema(x.toVector))
}

def solution1(data: Iterable[String]) =
	val schema = Schema.fromLines(data)
	// for each part, find all of the adjacent PartNumbers
	schema.parts.map(schema.findAdjacentPartNumbers(_))
		// merge the lists and avoid counting the same PartNumber twice
		.flatten.distinct
		.map(_.id)
		.sum


def solution2(data: Iterable[String]) =
	val schema = Schema.fromLines(data)
	// for each * part, find all of the _distinct_ adjacent PartNumbers. 
	schema.parts.filter(_.symbol == "*").map(schema.findAdjacentPartNumbers(_).distinct)
		// find only the parts with exactly two adjacent PartNumbers
		.filter(_.length == 2)
		// get the product of those two adjacent numbers
		.map(_.map(_.id).product)
		.sum


def main(args: Array[String]): Unit = {
	println("part1-sample: " ++ solution1(data1).DUMP)
	println("part1-answer: " ++ solution1(data2).DUMP)
	println("part2-sample: " ++ solution2(data1).DUMP)
	println("part2-answer: " ++ solution2(data2).DUMP)
}