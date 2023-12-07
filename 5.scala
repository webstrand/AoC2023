//> using dep org.scala-lang.modules:scala-parallel-collections_3:1.0.4
//> using dep org.scalatest:scalatest_3:3.3.0-SNAP4
import scala.collection.parallel.immutable.ParVector
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps
import scala.collection.mutable

implicit class Dumpable[A](x: A) {
	def DUMP[B] = runtime.ScalaRunTime.replStringOf(x, 10)
}

trait Tuple2[A, B] extends IndexedSeq[A | B]

val data1 = Source.fromString("""seeds: 79 14 55 13
	                            |
	                            |seed-to-soil map:
	                            |50 98 2
	                            |52 50 48
	                            |
	                            |soil-to-fertilizer map:
	                            |0 15 37
	                            |37 52 2
	                            |39 0 15
	                            |
	                            |fertilizer-to-water map:
	                            |49 53 8
	                            |0 11 42
	                            |42 0 7
	                            |57 7 4
	                            |
	                            |water-to-light map:
	                            |88 18 7
	                            |18 25 70
	                            |
	                            |light-to-temperature map:
	                            |45 77 23
	                            |81 45 19
	                            |68 64 13
	                            |
	                            |temperature-to-humidity map:
	                            |0 69 1
	                            |1 0 69
	                            |
	                            |humidity-to-location map:
	                            |60 56 37
	                            |56 93 4
	                            |""".stripMargin).mkString
val data2 = Source.fromFile("5.input").mkString

class Mapping(hit: Interval, miss: List[Interval])

// Half-open range [start, start + length)
// start = 5, length = 2
// range = [5, 6]
case class Interval(start: Long, end: Long) {	
	require(start < end, "Interval length must be non-zero, use Option[Interval] otherwise")
	override
	def toString = s"Interval($start, $length)"


	def length = end - start
	def offsetOf(x: Long) = 
		Option.when(x >= start && x < end)(x - start)

	def intersect(i: Interval) = 
		Interval.fromRange(Math.max(start, i.start), Math.min(end, i.end))

	def subtract(i: Interval) =
		Interval.fromRange(start, Math.min(end, i.start)) ++
		Interval.fromRange(Math.max(start, i.end), end)
}
object Interval {
	def fromLength(start: Long, length: Long) = fromRange(start, start + length)
	def fromRange(start: Long, end: Long) = Option.when(start < end)(Interval(start, end))
	implicit val intervalOrdering: Ordering[Interval] = Ordering.by(i => (i.start, i.end))
}

case class Work(mapped: List[Interval], unmapped: List[Interval])
case class Work1(mapped: Option[Interval], unmapped: List[Interval])

implicit class ListSelectOps(xs: List[Select])  {
	def partial = xs.foldLeft[PartialFunction[Long, Long]](PartialFunction.empty)((acc, x) => acc.orElse(x.partial)).orElse({ i => i })

	def select(i: Interval) = 
		var work = xs.foldLeft(Work(List(), List(i)))((work, select) => work match {
			case Work(mapped, unmapped) => if(unmapped.length == 0) Work(mapped, unmapped) else
				val work1s: List[Work1] = unmapped.map(x => select.remap(x))
				Work(mapped ++ (work1s.flatMap(_.mapped)), work1s.flatMap(_.unmapped))
		})
		work.mapped ++ work.unmapped
}

case class Almanac(
	seeds: List[Long],
	seed2soil: List[Select],
	soil2fert: List[Select],
	fert2water: List[Select],
	water2light: List[Select],
	light2temp: List[Select],
	temp2humid: List[Select],
	humid2loc: List[Select],
)
class Select(src: Interval, dst: Interval) {
	require(src.length == dst.length, "Interval length must be identical")
	override
	def toString = s"Select($src, $dst)"

	def partial: PartialFunction[Long, Long] = {
		case x if x >= src.start && x < src.start + src.length => x - src.start + dst.start
	}



	def remap(i: Interval) = 
		Work1(
			src.intersect(i).map(x => Interval.fromLength(dst.start + (x.start - src.start), x.length).get), 
			i.subtract(src).toList
		)


	//def stab(start, length): Option
}
object Almanac {
	private val format = raw"""(?sx)
	seeds:\s+([\d\s]+)\n\n
	seed-to-soil\smap:\s+([\d\s]+)\n\n
	soil-to-fertilizer\smap:\s+([\d\s]+)\n\n
	fertilizer-to-water\smap:\s+([\d\s]+)\n\n
	water-to-light\smap:\s+([\d\s]+)\n\n
	light-to-temperature\smap:\s+([\d\s]+)\n\n
	temperature-to-humidity\smap:\s+([\d\s]+)\n\n
	humidity-to-location\smap:\s+([\d\s]+)\n
	""".r
	def fromString(str: String) = str match {
		case format(seeds, seed2soil, soil2fert, fert2water, water2light, light2temp, temp2humid, humid2loc) => Almanac(
			raw"\d+".r.findAllIn(seeds).map(_.toLong).toList,
			parseMap(seed2soil),
			parseMap(soil2fert),
			parseMap(fert2water),
			parseMap(water2light),
			parseMap(light2temp),
			parseMap(temp2humid),
			parseMap(humid2loc),
		)
		case _ => throw Error(s"bad parse")
	}

	private val triple = raw"\s*(?<dst>\d+) (?<src>\d+) (?<length>\d+)\s*".r
	private def parseMap(str: String) = 
		triple.findAllMatchIn(str).map(x => Select(
			Interval.fromLength(x.group(2).toLong, x.group(3).toLong).get, 
			Interval.fromLength(x.group(1).toLong, x.group(3).toLong).get
		)).toList
}

def solution1(data: String) = 
	val a = Almanac.fromString(data)
	a.seeds
		.map(a.seed2soil.partial)
		.map(a.soil2fert.partial)
		.map(a.fert2water.partial)
		.map(a.water2light.partial)
		.map(a.light2temp.partial)
		.map(a.temp2humid.partial)
		.map(a.humid2loc.partial).min

def solution1_2(data: String) = 
	val a = Almanac.fromString(data)
	a.seeds.map(x => Interval(x, x + 1))
		.flatMap(a.seed2soil.select(_))
		.flatMap(a.soil2fert.select(_))
		.flatMap(a.fert2water.select(_))
		.flatMap(a.water2light.select(_))
		.flatMap(a.light2temp.select(_))
		.flatMap(a.temp2humid.select(_))
		.flatMap(a.humid2loc.select(_)).min.start

def solution2(data: String) =
	val a = Almanac.fromString(data)
	a.seeds.grouped(2).map { case Seq(start, length) => Interval.fromLength(start, length).get }.toList
		.flatMap(a.seed2soil.select(_))
		.flatMap(a.soil2fert.select(_))
		.flatMap(a.fert2water.select(_))
		.flatMap(a.water2light.select(_))
		.flatMap(a.light2temp.select(_))
		.flatMap(a.temp2humid.select(_))
		.flatMap(a.humid2loc.select(_)).min.start

def main(args: Array[String]): Unit = {
	println("part1-sample: " ++ solution1(data1).DUMP)
	println("part1-answer: " ++ solution1(data2).DUMP)
	println("part1_2-sample: " ++ solution1_2(data1).DUMP)
	println("part1_2-sample: " ++ solution1_2(data2).DUMP)
	println("part2-sample: " ++ solution2(data1).DUMP)
	println("part2-answer: " ++ solution2(data2).DUMP)
}