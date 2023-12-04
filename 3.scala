package day03
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

implicit class Dumpable[A](x: A) {
	def DUMP[B] = runtime.ScalaRunTime.replStringOf(x, 1000)
}

implicit class Product2Ops[A,B](x: Product2[A, B]) {
	def iter = x.productIterator.map(_.asInstanceOf[A | B])
}

implicit class Product3Ops[A,B,C](x: Product3[A, B, C]) {
	def iter = x.productIterator.map(_.asInstanceOf[A | B | C])
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
case class Cell(val row: Int, val col: Int, val part: String) {}

class Row(val columns: Vector[PartNumber | String]) extends Iterable[PartNumber | String] {
	def iterator: Iterator[PartNumber | String] = columns.iterator
	override
	def stringPrefix = "Row"

	def opposite(col: Int) = List(columns.lift(col - 1), columns.lift(col), columns.lift(col + 1))
	def adjacent(col: Int) = List(columns.lift(col - 1), columns.lift(col + 1))
}

class Schema(val rows: Vector[Row]) extends Iterable[Row] {
	def iterator: Iterator[Row] = rows.iterator
	override
	def stringPrefix = "Schema"

	def findPartNumber(row: Int, col: Int) = 
		List(
			rows.lift(row - 1).map(_.opposite(col)), 
			Some(rows.apply(row).adjacent(col)), 
			rows.lift(row + 1).map(_.opposite(col))
		).flatten.flatten.flatten.collect { case a: PartNumber => a }

	def parts = rows.map(_.columns.zipWithIndex.collect {
		case (s: String, i) if s != "." => (s, i)
	}).zipWithIndex.collect {
		case (v, row) if v.length != 0 => v.map { case (part, col) => Cell(row, col, part) }
	}.toList.flatten
}

val tokenizer = raw"\d+|\D".r
def parse(data: Iterable[String]) = data.zipWithIndex
	.map { case (l, i) =>
		tokenizer.findAllIn(l).zipWithIndex.flatMap {
			case (x, j) if x.head.isDigit => List.fill(x.length)(PartNumber(x.toInt, i, j))
			case (x, j) => List(x)
		}
		.pipe(x => new Row(x.toVector))
	}.pipe(x => new Schema(x.toVector))

def part(schema: Schema) = schema



def solution1(data: Iterable[String]) = parse(data).pipe(schema => schema.parts.map(c => schema.findPartNumber(c.row, c.col))).flatten.distinct.map(x => x.id).sum

def solution2(data: Iterable[String]) = parse(data).pipe(schema => 
	schema.parts.map(c => schema.findPartNumber(c.row, c.col).distinct).collect { case l if l.length == 2 => l }.map(l => l.foldLeft(1)(_ * _.id)).sum
)

def main(args: Array[String]): Unit = {
	println("part1-sample: " ++ solution1(data1).DUMP)
	println("part1-answer: " ++ solution1(data2).DUMP)
	println("part2-sample: " ++ solution2(data1).DUMP)
	println("part2-answer: " ++ solution2(data2).DUMP)
}