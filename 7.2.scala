import scala.util.chaining.scalaUtilChainingOps
import scala.io.Source
import scala.collection.immutable.HashSet

implicit class Dumpable[A](x: A) {
	def DUMP[B] = runtime.ScalaRunTime.replStringOf(x, 1000)
}

def solution2(data: Iterable[String]) = 
	Hand.multipleFromString(data).toList.sorted.zipWithIndex.map(x => x._1.bid * (x._2 + 1)).sum

def main(args: Array[String]): Unit = {
	println("part2-sample: " ++ solution2(data1).DUMP)
	println("part2-answer: " ++ solution2(data2).DUMP)
}

case class Card(char: Char, rank: Int) extends Ordered[Card] {
	override def toString = s"'$char"

	def compare(that: Card) = this.rank.compareTo(that.rank)
}

val power: Map[Char, Card] = "AKQT98765432J".toList.reverse.zipWithIndex.map(x => (x._1, Card(x._1, x._2))).toMap
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
	override def toString = s"Hand(${cards.toList.map(_.char).mkString}, ${bid})"

	val counts = cards.toList.groupMapReduce(x=>x)(x=>1)(_ + _)

	def handType = 
		val j = counts.get(power.apply('J')).getOrElse(0)
		val cnoj = counts.filter(_._1.char != 'J').toList.sortBy(-_._2)
		if (j == 5 || j == 4)
			HandType.FiveOfAKind
		else if (j == 3)
			if (cnoj.head._2 == 2)
				HandType.FiveOfAKind
			else
				HandType.FourOfAKind
		else if (j == 2)
			if(cnoj.head._2 == 3)
				HandType.FiveOfAKind
			else if(cnoj.head._2 == 2)
				HandType.FourOfAKind
			else
				HandType.ThreeOfAKind
		else if (j == 1)
			if (cnoj.head._2 == 4)
				HandType.FiveOfAKind
			else if (cnoj.head._2 == 3)
				HandType.FourOfAKind
			else if (cnoj.head._2 == 2)
				if (cnoj.apply(1)._2 == 2)
					HandType.FullHouse
				else
					HandType.ThreeOfAKind
			else
				HandType.OnePair
		else cnoj.apply(0) match {
			case (card, 5) => HandType.FiveOfAKind
			case (card, 4) => HandType.FourOfAKind
			case (card, 3) => cnoj.apply(1) match {
				case (card, 2) => HandType.FullHouse
				case _ => HandType.ThreeOfAKind
			}
			case (card, 2) => cnoj.apply(1) match {
				case (card, 2) => HandType.TwoPair
				case _ => HandType.OnePair
			}
			case _ => HandType.HighCard
		}

	def compare(that: Hand) =
		handType.compareTo(that.handType)
			.orElse(cards._1.rank.compareTo(that.cards._1.rank))
			.orElse(cards._2.rank.compareTo(that.cards._2.rank))
			.orElse(cards._3.rank.compareTo(that.cards._3.rank))
			.orElse(cards._4.rank.compareTo(that.cards._4.rank))
			.orElse(cards._5.rank.compareTo(that.cards._5.rank))
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