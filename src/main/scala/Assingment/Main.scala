package Assingment

import scala.io.Source
import scala.util.control.*
import scala.collection.mutable.ListBuffer

object Main {

  def main(args: Array[String]): Unit = {

    case class Movies(imdb_title_id1: String, title1: String, year1: String, drct: String, cntry: List[String], lang: List[String], bdgt: String, durtn: String, usrRvws: String, votes: String) {
      var imdb_title_id: String = imdb_title_id1
      var title: String = title1
      var year: Int = year1.toInt
      var director: String = drct
      var country: List[String] = cntry
      var language: List[String] = lang
      var budget: Int = bdgt.filter(_.isDigit).toInt
      var duration: Int = durtn.toInt
      var userReviews: Int = usrRvws.toInt
      var vote: Int = votes.toInt
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
            var language = list2(8).replace(" ", "")
            var languageList = language.replace("\"", "").split(",").toList
            if (list2.size >= 20) {
              var movie = new Movies(list2(0), list2(1), list2(3), list2(9), countryList, languageList, list2(16), list2(6), list2(20), list2(15))
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
    var languageSet = Set[String]()
    for (mapElemnt <- movieList) {
      countrySet = countrySet.++(mapElemnt.country.toSet)
      languageSet = languageSet.++(mapElemnt.language.toSet)
    }

    //  println(laguageSet.toList.sortBy(r=>r))
    def moviesByCountry(): Unit = {

      for (country <- countrySet) {
        //      var mov = movieList.groupBy(c => c.country(c.country.indexOf(country)))
        var mov = movieList.filter(_.country.contains(country))
        mov = mov.sortBy(b => b.budget).reverse
        //        println(s"${country} =>  ${mov(0).budget} ${mov(0).title}")
        //        println("-----------------------------------------------------------------")
      }
    }

    moviesByCountry()

    //    Milestone 4

    def longestMovieCountryWise(minVotes: Int): ListBuffer[Movies] = {
      var finalList = ListBuffer[Movies]()
      for (country <- countrySet) {
        var mov = movieList.filter(_.country.contains(country)).filter(_.vote >= minVotes)
        mov = mov.sortBy(b => b.duration).reverse
        finalList += mov(0)
      }
      return finalList
    }

    var longestMovieList = longestMovieCountryWise(100)
    //    longestMovieList.foreach(println)

    //MileStone 5

    def languageWiseMovieCount(country: String, minBudget: Int, maxBudget: Int): ListBuffer[ListBuffer[Movies]] = {
      var finalList = ListBuffer[ListBuffer[Movies]]()
      var movv = movieList.filter(_.country.contains(country))
      for (language <- languageSet) {
        var mov = movv.filter(_.language.contains(language)).filter(_.budget >= minBudget).filter(_.budget <= maxBudget)
        finalList+=mov
//        println(s"${language} ${mov.length}")
      }
        return finalList.sortBy(r=> r.size).reverse

    }
    var languageWiseMovieList = languageWiseMovieCount("USA", 1000, 900000000)
  }
}