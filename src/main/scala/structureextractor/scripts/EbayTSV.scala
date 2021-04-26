package structureextractor.scripts

import structureextractor.Util.printToFile

import java.io.File
import scala.annotation.tailrec
import scala.io.Source

object EbayTSV {

	val pattern = """❲(\w+)❳❮([^❯]*)❯""".r

	sealed trait Property

	case class ID(value: String) extends Property

	case class Name(value: String) extends Property

	case class SoldOn(value: String) extends Property

	case class Price(value: String) extends Property

	case class TotalPrice(value: String) extends Property

	def findMatches(line: String): List[Property] = pattern.findAllMatchIn(line).map({ m =>
		if (m.group(1) == "id")
			ID(m.group(2))
		else if (m.group(1) == "name")
			Name(m.group(2))
		else if (m.group(1) == "soldon")
			SoldOn(m.group(2))
		else if (m.group(1) == "itemprice")
			Price(m.group(2))
		else if (m.group(1) == "totalprice")
			TotalPrice(m.group(2))
		else
			throw new Exception(s"Unexpected property name '${m.group(1)}''")
	}).toList

	case class Sale(price: String, date: String, itemIds: List[String], itemNames: List[String])

	@tailrec
	def process(matches: List[Property],
	            acc: List[Sale] = List.empty, thisGroup: List[Property] = List.empty)
	: List[Sale] = {

		def addSale(sales: List[Sale], group: List[Property]): List[Sale] =
			group.reverse match {
				case TotalPrice(price) :: SoldOn(date) :: rest =>
					Sale(price,
						date,
						rest flatMap { case ID(id) => List(id); case _ => Nil },
						rest flatMap { case Name(name) => List(name); case _ => Nil }
					) :: sales
				case Nil =>
					sales
				case _ =>
					println(s"Discarding group without price and/or date: $group")
					sales
			}

		matches match {
			case Nil => addSale(acc, thisGroup)
			case Name(name) :: ID(id) :: Price(price) :: SoldOn(soldOn) :: rest =>
				process(rest,
					Sale(price, soldOn, List(id), List(name)) :: addSale(acc, thisGroup), List.empty)
			case TotalPrice(price) :: SoldOn(soldOn) :: rest =>
				process(rest, addSale(acc, thisGroup), List(SoldOn(soldOn), TotalPrice(price)))
			case Name(name) :: ID(id) :: rest =>
				process(rest, acc, Name(name) :: ID(id) :: thisGroup)
			case ID(id) :: rest => // apparently rewgic-sold-2020-04-18-p2.html has a nameless item?
				println(s"Warning: adding nameless item ID $id")
				process(rest, acc, Name("???") :: ID(id) :: thisGroup)
			case _ :: rest =>
				println("Unable to process: " + matches.take(3))
				process(rest, addSale(acc, thisGroup), List.empty)
		}
	}

	def main(args: Array[String]): Unit = {
		val inputPath = args(0)
		val outputPath = args(1)

		val source = Source.fromFile(inputPath)
		val matches: List[Property] = try source.getLines.flatMap(findMatches).toList
		finally source.close

		val sales = process(matches).reverse

		printToFile(new File(outputPath)) { writer =>
			for (sale <- sales) {
				writer.println(List(sale.price, sale.date,
					sale.itemIds.mkString(";"),
					sale.itemNames.mkString(";")
				).mkString("\t"))
			}
		}
	}
}
