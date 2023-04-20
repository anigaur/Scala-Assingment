package Assingment

import scala.io.Source
import scala.util.control.*
import scala.collection.mutable.ListBuffer

object Main {

  def main(args: Array[String]): Unit = {

    case class Movies(imdb_title_id1: String, title1: String, year1: String, drct: String, cntry: List[String], lang: String, bdgt: String, durtn: String, usrRvws: String) {
      var imdb_title_id: String = imdb_title_id1
      var title: String = title1
      var year: Int = year1.toInt
      var director: String = drct
      var country: List[String] = cntry
      var language: String = lang
      var budget: Int = bdgt.filter(_.isDigit).toInt
      var duration: Int = durtn.toInt
      var userReviews: Int = usrRvws.toInt
    }


    val fileName = "resources/imdb_movies.csv"
    var i = 0
    var movieIndex = 0
    var movieArray = new Array[Movies](10000)
    var movieList = new ListBuffer[Movies]()
    var loop = new Breaks

    def countWithFilter(string: String, char: Char): Int = string.filter(_ == char).size

    loop.breakable {

      for (line <- Source.fromFile(fileName).getLines) {
        if (movieIndex < 10000) {
          if (i == 0) {
          }
          else {

            var tempList = line.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)").toList
            var list2 = tempList.map { e => if (e == "") "0" else e }
            var country = list2(7).replace(" ", "")
            var countryList = country.replace("\"", "").split(",").toList
            if (list2.size >= 20) {
              var movie = new Movies(list2(0), list2(1), list2(3), list2(9), countryList, list2(8), list2(16), list2(6), list2(20))
              movieArray(movieIndex) = movie
              movieList += movie
              movieIndex += 1

            }
          }
        }
        else {
          loop.break
        }
        i += 1
      }

    }


    // milestone 1
    def titleByDirector(director: String, lowerLimit: Int, upperLimit: Int): ListBuffer[String] = {
      var titles = ListBuffer[String]()
      for (movieElemnt <- movieArray) {
        if ((movieElemnt.director.contains(director)) && (movieElemnt.year >= lowerLimit) && (movieElemnt.year <= upperLimit)) {
          titles += movieElemnt.title

        }
      }
      return titles
    }

    var titleList = titleByDirector("Alfred E. Green", 1930, 2000)
    //    println(titleList.toString())

    //milestone 2

    def moviesWithHighestReview(review: Int): ListBuffer[String] = {
      var titles = ListBuffer[String]()
      movieList = movieList.sortBy(_.userReviews).reverse
      for (movieElemnt <- movieList) {
        if (movieElemnt.userReviews > review) {
          titles += movieElemnt.title
        }
      }
      return titles
    }

    var titleByReview = moviesWithHighestReview(2)
    //    println(titleByReview)

    //  milestone3

    var countrySet = Set[String]()
    for (mapElemnt <- movieList) {
      countrySet = countrySet.++(mapElemnt.country.toSet)
    }

    def moviesByCountry(): Unit = {

      for (country <- countrySet) {
        //      var mov = movieList.groupBy(c => c.country(c.country.indexOf(country)))
        var mov = movieList.filter(_.country.contains(country))
        mov = mov.sortBy(b => b.budget).reverse
        println(s"${country} =>  ${mov(0).budget} ${mov(0).title}")
        println("-----------------------------------------------------------------")
      }
    }

    moviesByCountry()

  }
}




