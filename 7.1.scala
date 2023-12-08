import scala.util.chaining.scalaUtilChainingOps
import scala.io.Source
import scala.collection.immutable.HashSet

implicit class Dumpable[A](x: A) {
	def DUMP[B] = runtime.ScalaRunTime.replStringOf(x, 1000)
}

def solution1(data: Iterable[String]) = 
	Hand.multipleFromString(data).toList.sorted.zipWithIndex.map(x => x._1.bid * (x._2 + 1)).sum

def main(args: Array[String]): Unit = {
	println("part1-sample: " ++ solution1(data1).DUMP)
	println("part1-answer: " ++ solution1(data2).DUMP)
}

case class Card(char: Char, rank: Int) extends Ordered[Card] {
	override def toString = s"'$char"

	def compare(that: Card) = this.rank.compareTo(that.rank)
}

val power: Map[Char, Card] = "AKQJT98765432".toList.reverse.zipWithIndex.map(x => (x._1, Card(x._1, x._2))).toMap
//val power = "AKQJT98765432".toList.reverse.zipWithIndex.map(x => Card(x._1, x._2))

enum HandType(val order: Int) extends Ordered[HandType]:
  case FiveOfAKind extends HandType(7)
  case FourOfAKind extends HandType(6)
  case FullHouse extends HandType(5)
  case ThreeOfAKind extends HandType(4)
  case TwoPair extends HandType(3)
  case OnePair extends HandType(2)
  case HighCard extends HandType(1)

  override def compare(that: HandType): Int = this.order.compare(that.order)


extension (x: Int) {
	inline def orElse[A](fn: => A): A | Int = if (x == 0) fn else x
}

case class Hand(cards: Tuple5[Card, Card, Card, Card, Card], bid: Int) extends Ordered[Hand] {
	override def toString = s"Hand(${cards.toList.map(_.char).mkString}, ${bid}, ${handType})"

	val counts = cards.toList.groupMapReduce(x=>x)(x=>1)(_ + _).toList.sortBy(-_._2)
	
	def handType = counts.apply(0) match {
		case (card, 5) => HandType.FiveOfAKind
		case (card, 4) => HandType.FourOfAKind
		case (card, 3) => counts.apply(1) match {
			case (card, 2) => HandType.FullHouse
			case _ => HandType.ThreeOfAKind
		}
		case (card, 2) => counts.apply(1) match {
			case (card, 2) => HandType.TwoPair
			case _ => HandType.OnePair
		}
		case _ => HandType.HighCard
	}

	def compare(that: Hand) = 
		handType.compareTo(that.handType).orElse(cards.compareTo(that.cards))
}
object Hand {
	def fromString(str: String) = str match {
		case s"$cards $bid" if cards.size == 5 => 
			Hand((
				power.apply(cards.apply(0)), 
				power.apply(cards.apply(1)), 
				power.apply(cards.apply(2)), 
				power.apply(cards.apply(3)), 
				power.apply(cards.apply(4))
			), bid.toInt)
		// Hand(cards.map(power.apply(_)).groupMapReduce(x=>x)(x=>1)(_ + _), bid.toInt)
		case _ => throw Error(s"malformed $str")
	}

	def multipleFromString(str: Iterable[String]) = str.map(Hand.fromString)
}

val data1 = Source.fromString("""32T3K 765
	                            |T55J5 684
	                            |KK677 28
	                            |KTJJT 220
	                            |QQQJA 483
	                            |""".stripMargin).getLines.toList
val data2 = Source.fromFile("7.input").getLines.toList