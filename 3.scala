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

case class PartNumber(val id: Int, val row: Int, val col: Int) {
	override
	def toString = s"PartNumber($id)"
}
case class Part(val name: String, val row: Int, val col: Int) {}

class Row(val columns: Vector[PartNumber | String]) extends Iterable[PartNumber | String] {
	def iterator: Iterator[PartNumber | String] = columns.iterator
	override
	def stringPrefix = "Row"

	def xxx(col: Int) = List(columns.lift(col - 1), columns.lift(col), columns.lift(col + 1))
	def x_x(col: Int) = List(columns.lift(col - 1), columns.lift(col + 1))
}

class Schema(val rows: Vector[Row]) extends Iterable[Row] {
	def iterator: Iterator[Row] = rows.iterator
	override
	def stringPrefix = "Schema"

	def findAdjacentPartNumbers(row: Int, col: Int) = 
		List(
			rows.lift(row - 1).map(_.xxx(col)), 
			rows.lift(row    ).map(_.x_x(col)), 
			rows.lift(row + 1).map(_.xxx(col)),
		).flatten.flatten.flatten.collect { case a: PartNumber => a }

	def parts =
		// Visit every row
		rows.map(
			// Visit every column and pick out only the part symbols
			_.columns.zipWithIndex
				.collect { case (s: String, i) if s != "." => (s, i) }
		).zipWithIndex
		.collect {
			case (v, row) if v.length != 0 => 
				v.map { case (part, col) => Part(part, row, col) }
		}.toList.flatten
}

object Schema {
	private val tokenizer = raw"\d+|\D".r

	def fromLines(data: Iterable[String]) = 
		data.zipWithIndex
			.map { case (l, i) =>
				tokenizer.findAllIn(l).zipWithIndex.flatMap {
					case (x, j) if x.head.isDigit => List.fill(x.length)(PartNumber(x.toInt, i, j))
					case (x, j) => List(x)
				}
				.pipe(x => new Row(x.toVector))
			}.pipe(x => new Schema(x.toVector))
}

def solution1(data: Iterable[String]) =
	val schema = Schema.fromLines(data)
	// for each part, find all of the adjacent PartNumbers
	schema.parts.map(c => schema.findAdjacentPartNumbers(c.row, c.col))
		// merge the lists and avoid counting the same PartNumber twice
		.flatten.distinct
		.map(x => x.id)
		.sum


def solution2(data: Iterable[String]) = 
	val schema = Schema.fromLines(data)
	// for each part, find all of the _distinct_ adjacent PartNumbers. 
	schema.parts.map(c => schema.findAdjacentPartNumbers(c.row, c.col).distinct)
		// find only the parts with exactly two adjacent PartNumbers
		.collect { case l if l.length == 2 => l }
		// get the product of those two adjacent numbers
		.map(l => l.foldLeft(1)(_ * _.id))
		.sum


def main(args: Array[String]): Unit = {
	println("part1-sample: " ++ solution1(data1).DUMP)
	println("part1-answer: " ++ solution1(data2).DUMP)
	println("part2-sample: " ++ solution2(data1).DUMP)
	println("part2-answer: " ++ solution2(data2).DUMP)
}