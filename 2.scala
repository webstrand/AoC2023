package day02
import scala.io.Source.fromFile
import scala.util.chaining.scalaUtilChainingOps
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

val data1 = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""; // 8

val data2 = fromFile("2.input").mkString

val tokenizer = raw"Game\s(\d+)|([:;])|(\d+) (\w+)".r
case class Game(id: Int, rounds: Stack[Map[String, Int]])
def parse(str: String) =
	tokenizer.findAllMatchIn(str)
	.foldLeft[Queue[Game]](Queue())((s, m) => m.subgroups match {
		case List(id, null, null, null) => 
			s.enqueue(Game(id.toInt, Stack()))
		case List(null, _, null, null) => 
			s.last.rounds.push(Map())
			s
		case List(null, null, count, color) => count.toInt.pipe(count =>
				s.last.rounds.push(
					s.last.rounds
						.pop()
						.pipe(round =>
							round.get(color).pipe((item: Option[Int]) => item match {
								case Some(c) if c > count => round
								case _ => round.updated(color, count)
							})
						)
				)
			)
			s

		case _ => throw new RuntimeException("Unresolvable Regex subgroups pattern!")
	}).toList
	

// Merge the various rounds in a Game into one Map showing the maximum count of every color
// that we've seen.
case class Stats(id: Int, seen: Map[String, Int])
def maxStats(game: Game) =
	Stats(
		game.id,
		// Merge bags in each round to make round summaries, then merge those round summaries
		game.rounds.foldLeft(Map[String, Int]())((acc, round) =>
			round.foldLeft(acc)((acc, entry) => entry match { case (color, count) =>
				acc.get(color) match {
					case Some(c) if c > count => acc
					case _ => acc.updated(color, count)
				}
			})
		)
	)


def solution1(data: String, compare: Map[String, Int]) =
	parse(data).map(maxStats(_)).filter(s => compare.foldLeft(true)((matches, entry) => 
		matches match {
			case false => false
			case true => entry match { case (color, maxCount) => 
				s.seen.get(color) match {
					case Some(count) if count <= maxCount => true
					case _ => false
				}
			}
		}

	))
	.map(_.id)
	.sum

def solution2(data: String) =
	parse(data).map(maxStats(_).seen.foldLeft(1)(_ * _._2)).sum


@main def main(): Unit = {
	println("part1-sample: " ++ runtime.ScalaRunTime.replStringOf(solution1(data1, Map("red" -> 12, "green" -> 13, "blue" -> 14)), 15))
	println("part1-answer: " ++ runtime.ScalaRunTime.replStringOf(solution1(data2, Map("red" -> 12, "green" -> 13, "blue" -> 14)), 15))
	println("part2-sample: " ++ runtime.ScalaRunTime.replStringOf(solution2(data1), 15))
	println("part2-answer: " ++ runtime.ScalaRunTime.replStringOf(solution2(data2), 15))
}