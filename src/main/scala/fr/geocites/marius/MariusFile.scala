/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.geocites.marius

import fr.geocites.marius.model.City

import scala.collection.mutable
import scala.io.{ Codec, Source }
import data._

object MariusFile {

  private lazy val memoization = new mutable.HashMap[Int, DistanceMatrix]

  def memoize(census: Int)(f: => DistanceMatrix) =
    memoization.getOrElseUpdate(census, f)

  def numberOfCensus = 6

  def contentCities = {
    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("fr/geocites/marius/marius.csv"))(Codec.UTF8)

    input.getLines.map {
      l => l.split(",").toSeq
    }
  }

  def data = contentCities.drop(1).toList

  def numberOfDates(census: Int) = numberOfCensus - census

  def startingCities(census: Int) =
    data.filter {
      _.takeRight(numberOfDates(census)).forall(!_.isEmpty)
    }

  def latitudes(census: Int) = startingCities(census).map(_(4).toDouble)
  def longitudes(census: Int) = startingCities(census).map(_(5).toDouble)

  def positions(census: Int) = (longitudes(census) zip latitudes(census))

  /** Cache of the distance matrix between */
  def distanceMatrix(census: Int): DistanceMatrix = memoize(census) { DistanceMatrix(positions(census)) }

  def apply(census: Int) = new MariusFile(census)
}

class MariusFile(census: Int) {

  /** Read the content of the file */
  def contentCities = MariusFile.contentCities

  /** Read the header of the csv file */
  def header = contentCities.next

  /** Read the data part of the csv file */
  def data = MariusFile.data(census)

  /** The number of columns of census data */
  def numberOfDates = MariusFile.numberOfDates(census)

  /** The dates of the census */
  lazy val dates = header.takeRight(numberOfDates).map(_.toInt)

  def firstDate = dates.head

  /** The cities with known populations for all dates */
  def startingCities = MariusFile.startingCities(census)

  /** Number of cities taken into account */
  def nbCities = startingCities.size

  /** Read the position of the cities */
  def positions = MariusFile.positions(census)

  /** Number of column before the census columns */
  def columnsBeforeDates = header.size - numberOfDates

  /**
   * Column of population at a given date
   *
   * @param date date of observation
   * @return an option containing the population if provided, none otherwise
   */
  def populations(date: Int): Option[Seq[Double]] =
    dates.indexOption(date).map {
      c => startingCities.map(_(columnsBeforeDates + c).toDouble)
    }

  /** Id of cities */
  def arokatos = startingCities.map(_(0))

  /** Names of the cities */
  def names = startingCities.map(_(1))

  /** Latitudes of the cities in decimal degrees */
  def latitudes = MariusFile.latitudes(census)

  /** Longitudes of the cities in decimal degrees */
  def longitudes = MariusFile.longitudes(census)

  /** Populations of the cities at the first date */
  def initialPopulations = populations(dates.head).get

  /** Cities with oil and/or gaz */
  def oilOrGazDistribution = startingCities.map(l => toBoolean(l(8)))

  /** Cities with coal */
  def coalDistribution = startingCities.map(l => toBoolean(l(6)))

  /** Regions of the cities */
  def cityRegions = startingCities.map(_(2)).toIterator

  /** A vector of boolean, true in case a city is a regional capital */
  def regionalCapitals = startingCities.map(l => toBoolean(l(7))).toIterator

  /** A vector of boolean, true in case a city is a national capital */
  def nationalCapitals = startingCities.map(l => toBoolean(l(9))).toIterator

  /** States cities belong to */
  def cityNations = startingCities.map(_(3)).toIterator

  /** Cache of the distance matrix between */
  lazy val distanceMatrix: DistanceMatrix = MariusFile.distanceMatrix(census)

  /** Read the content of the file */
  def contentRegions = {
    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("fr/geocites/marius/marius-regions.csv"))(Codec.UTF8)

    input.getLines.map {
      l => l.split(",").toSeq
    }
  }

  /** Read the header of the csv file */
  def headerRegions = contentRegions.next

  /** Read the data part of the csv file */
  def regionData = contentRegions.drop(1).toList

  def initialUrbanisationRates = regionData.map { _.takeRight(numberOfDates).head.toDouble / 100.0 }

  /** Id of regions */
  def regionIDs = regionData.map(_(0))

  /** Names of the regions */
  def regionNames = regionData.map(_(1))

  def cities(populationToWealthExponent: Double) = {
    def initialWealth(population: Double): Double = math.pow(population, populationToWealthExponent)
    def rescaleWealth(wealth: Seq[Double], population: Seq[Double]) = {
      val factor = population.sum / wealth.sum
      wealth.map(_ * factor)
    }

    val pop = initialPopulations.toSeq
    val initialWealths = rescaleWealth(pop.map(initialWealth), pop)

    val cities =
      for {
        line <- (pop.toIterator zip cityRegions zip cityNations zip regionalCapitals zip nationalCapitals zip oilOrGazDistribution.toIterator zip coalDistribution.toIterator zip initialWealths.toIterator)
      } yield {
        val (((((((population,
          region),
          nation),
          regionalCapital),
          nationalCapital),
          oilOrGaz),
          coal),
          initialWealth) = line

        City(
          population = population,
          region = region,
          nation = nation,
          regionalCapital = regionalCapital,
          nationalCapital = nationalCapital,
          wealth = initialWealth,
          oilOrGaz = oilOrGaz,
          coal = coal
        )
      }
    cities.take(nbCities).toVector
  }

  /** A converter function from string to boolean */
  private def toBoolean(s: String) =
    s match {
      case "1" => true
      case _ => false
    }

}