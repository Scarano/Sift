package crawler

import java.net.URL
import org.jsoup.Jsoup
import scala.jdk.CollectionConverters.CollectionHasAsScala

import structureextractor.Util.abbreviate


object Crawler {
	def fetch(url: URL): (String, List[URL]) = {
		val doc = Jsoup.connect(url.toString).get()
		val anchors = doc.select("a")
		val links = anchors.asScala.map(_.attr("href")).filter(_.nonEmpty)
		val linkUrls = links.map(new URL(url, _))
		(doc.outerHtml(), linkUrls.toList)
	}


	def main(args: Array[String]): Unit = {
		val url = new URL("https://www.microcenter.com/category/4294967292/all-desktops?rd=1&vkw=computer")
		val (content, newUrls) = fetch(url)
		println(abbreviate(content, 200))
		println(newUrls.take(10).mkString("\n"))
	}
}

