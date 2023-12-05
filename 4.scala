import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps
import scala.collection.mutable

implicit class Dumpable[A](x: A) {
	def DUMP[B] = runtime.ScalaRunTime.replStringOf(x, 1000)
}

val data1 = Source.fromString(
	"""Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
	  |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
	  |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
	  |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
	  |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
	  |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
	  |""".stripMargin).getLines().toList

val data2 = Source.fromFile("4.input").getLines().toList


case class Card(id: Int, winning: Set[Int], picks: Set[Int]) {
	override
	def toString = s"Card($id)"

	def matched = winning.intersect(picks)
}
object Card {
	private val tokenizer = raw"\d+".r
	private val struct = raw"Card\s+(\d+):([\d ]+)\|([\d ]+)".r
	def fromString(str: String) = str match {
		case struct(id, winning, picks) => Card(
			id.toInt,
			tokenizer.findAllIn(winning).map(_.toInt).toSet,
			tokenizer.findAllIn(picks).map(_.toInt).toSet
		)
		case _ => throw Error(s"bad line $str")
	}

	def fromLines(lines: Iterable[String]) =
		lines.map(Card.fromString)
}

case class Deck(cards: Vector[Card]);
object Deck {
	def fromLines(lines: Iterable[String]) = Deck(lines.map(Card.fromString(_)).toVector)
}

def solution1(data: Iterable[String]) =
	Deck.fromLines(data).cards
		.map(_.matched.size)
		.map(x => if (x == 0) 0 else 1 << (x - 1))
		.sum

def solution2(data: Iterable[String]) =
	val cards = Deck.fromLines(data).cards
	val sums = new mutable.HashMap[Card, Int] ++ cards.map((_ -> 1))
	cards.foreach((card) => cards
		.drop(card.id)
		.take(card.matched.size)
		.foreach(target => sums.update(target, sums.apply(target) + sums.apply(card)))
	)
	sums.values.sum


def main(args: Array[String]): Unit = {
	println("part1-sample: " ++ solution1(data1).DUMP)
	println("part1-answer: " ++ solution1(data2).DUMP)
	println("part2-sample: " ++ solution2(data1).DUMP)
	println("part2-answer: " ++ solution2(data2).DUMP)
}