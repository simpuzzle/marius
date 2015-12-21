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

import fr.geocites.marius.data._
import fr.iscpif.simpuzzle.puzzle.Puzzle

import scalaz._
import Scalaz._

object model {

  case class Activity(sizeEffectOnDemand: Double, sizeEffectOnSupply: Double, exchangeRate: Double)

  case class MariusState(
    step: Int,
    cities: Vector[City],
    network: Network,
    distanceMatrix: DistanceMatrix)

  case class City(
    population: Double,
    wealth: Double,
    region: String,
    nation: String,
    regionalCapital: Boolean,
    nationalCapital: Boolean,
    oilOrGaz: Boolean,
    coal: Boolean)

  case class Interaction(from: Int, to: Int, transacted: Double)

  sealed trait Error
  case class AssertionFailed(msg: String) extends Exception(msg) with Error

  val model = Puzzle[MariusState, Interaction, Error]
  import model._

  def marius(
    transactions: Transactions,
    interactionPotential: InteractionPotential,
    balance: Balance,
    economicMultiplier: Double,
    wealthToPopulationExponent: Double,
    activities: Vector[Activity])(implicit log: Logger = Logger.empty): Step[Vector[City]] = {

    def wealthToPopulation(wealth: Double) =
      if (wealth >= 0) success(math.pow(wealth, wealthToPopulationExponent))
      else failure(AssertionFailed(s"Negative wealth $wealth"))

    def populations(newWealths: Vector[Double], state: MariusState): Validate[Vector[Double]] =
      (state.cities zip newWealths).zipWithIndex.traverse { case ((city, newWealth), i) => population(city, newWealth, i) }

    def wealths(deltas: Vector[Double], state: MariusState): Vector[Double] =
      (state.cities.map(_.wealth) zip deltas) map Function.tupled(_ + _)

    def population(city: City, newWealth: Double, index: Int): Validate[Double] =
      for {
        _ <- if (newWealth < 0) failure(AssertionFailed(s"City $index error in wealth before conversion toPop $newWealth")) else success()
        newPop <- wealthToPopulation(newWealth)
        oldPop <- wealthToPopulation(city.wealth)
        deltaPopulation = (newPop - oldPop) / economicMultiplier
        newPopulation = city.population + deltaPopulation
        _ <- if (newPopulation < 0) failure(AssertionFailed(s"Population is negative for city $index: $newPopulation")) else success()
      } yield newPopulation

    def newState(newPopulations: Vector[Double], newWealths: Vector[Double]) = State { state: MariusState =>
      val newCities = (state.cities zip newPopulations zip newWealths).map {
        case ((city, newPopulation), newWealth) => city.copy(population = newPopulation, wealth = newWealth)
      }
      val newState = state.copy(step = state.step + 1, cities = newCities)
      (newState, newCities)
    }

    for {
      state <- Step()
      deltas <- step(deltaWealths(transactions, interactionPotential, balance, economicMultiplier, activities, state))
      newWealths <- step(wealths(deltas, state))
      newPopulations <- step(populations(newWealths, state))
      newCities <- newState(newPopulations, newWealths)
    } yield newCities

  }

  def deltaWealths(
    transactions: Transactions,
    potential: InteractionPotential,
    balance: Balance,
    economicMultiplier: Double,
    activities: Vector[Activity],
    state: MariusState)(implicit log: Logger): Log[Vector[Double]] = {
    def populations = state.cities.map(_.population)

    def delta(activity: Activity) = {
      val suppliesOfCities = supplies(populations, economicMultiplier, activity)
      val demandsOfCities = demands(populations, economicMultiplier, activity)
      deltaWealth(transactions, potential, balance)(state.distanceMatrix, state.network, state.cities, suppliesOfCities, demandsOfCities)
    }

    for {
      a <- activities.traverse[Log, Vector[Double]](delta)
    } yield a.transpose.map(_.sum)
  }

  def deltaWealth(
    transactions: Transactions,
    potential: InteractionPotential,
    balance: Balance)(
      distances: DistanceMatrix,
      network: Network,
      cities: Vector[City],
      supplies: Vector[Double],
      demands: Vector[Double])(implicit log: Logger) = {
    def populations = cities.map(_.population)

    val t =
      Transacted(
        cities,
        supplies,
        demands,
        transactions(distances, network, supplies, demands, potential)
      )

    def interactions =
      for {
        (row, i) <- t.transacted.lines.zipWithIndex
        Cell(j, value) <- row
      } yield Interaction(i, j, value)

    (balance(t) zip supplies zip demands) map {
      case ((balance, supply), demand) => supply - demand + balance
    }

    log(balance(t), interactions.toVector)
  }

  case class Transacted(cities: Vector[City], supplies: Vector[Double], demands: Vector[Double], transacted: Matrix) {
    lazy val transposedTransacted = transacted.transpose
    lazy val transactedFromSum = transacted.linesContent.map(_.sum)
    lazy val transactedToSum = transposedTransacted.linesContent.map(_.sum)
  }

  type Balance = Reader[Transacted, Vector[Double]]

  implicit class BalanceOperations(b1: Balance) {
    def +(b2: Balance) =
      for {
        l <- b1
        r <- b2
      } yield (l zip r) map Function.tupled(_ + _)

    def -(b2: Balance) =
      for {
        l <- b1
        r <- b2
      } yield (l zip r) map Function.tupled(_ - _)
  }

  def unsolds: Balance = Reader { t => for { (supply, i) <- t.supplies.zipWithIndex } yield supply - t.transactedFromSum(i) }
  def unsatisfieds: Balance = Reader { t => for { (demand, i) <- t.demands.zipWithIndex } yield demand - t.transactedToSum(i) }

  def bonusFixedCostsBalance(fixedCost: Double, bonusMultiplier: Double): Balance = Reader { t =>
    def fixedCosts: Seq[Double] =
      t.transacted.linesContent.map { _.count(_ > 0.0) * fixedCost }

    def bonuses: Vector[Double] = {
      def diversityBonuses = {
        def transactedWith(transacted: Vector[Cell]) =
          transacted.filter { case Cell(_, v) => v > 0 }.map { case Cell(to, _) => to }

        (t.transacted.lines zip t.transposedTransacted.lines) map {
          case (from, to) => (transactedWith(from.toVector).toSet union transactedWith(to.toVector).toSet).size / t.cities.size.toDouble
        }
      }

      def importVolumes: Vector[Double] =
        for { (demand, i) <- t.demands.zipWithIndex } yield t.transactedToSum(i)

      def exportVolumes: Vector[Double] =
        for { (supply, i) <- t.supplies.zipWithIndex } yield t.transactedFromSum(i)

      (importVolumes zip exportVolumes zip diversityBonuses).map {
        case ((importVolume, exportVolume), diversityBonus) =>
          bonusMultiplier * (importVolume + exportVolume) * diversityBonus
      }
    }

    (bonuses zip fixedCosts).map { case (bonus, fixedCosts) => bonus - fixedCosts }
  }

  type Transactions = (DistanceMatrix, Network, Vector[Double], Vector[Double], InteractionPotential) => Matrix

  /** Filter the interaction potential matrix */
  def fixedCostTransactions(fixedCost: Double): Transactions = (
    distances: DistanceMatrix,
    network: Network,
    supplies: Vector[Double],
    demands: Vector[Double],
    potential: InteractionPotential) => {

    def interactionPotentials(distances: DistanceMatrix, network: Network, supplies: Vector[Double], demands: Vector[Double], potential: InteractionPotential) = {
      val iM1 = supplies.toArray
      val iM2 = demands.toArray
      network.mapNodes { (i, j) => potential(iM1(i), iM2(j), distances(i)(j)) }
    }

    val interactionMatrixValue = interactionPotentials(distances, network, supplies, demands, potential)
    val fromInteractionPotentialSum = interactionMatrixValue.transpose.linesContent.map(_.sum)

    interactionMatrixValue.map {
      (from, to, interactionPotential) =>
        if (interactionPotential > 0) {
          val fSupply = supplies(from)
          val fromIPSum = fromInteractionPotentialSum(from)
          val normalisedIPFrom = interactionPotential / fromIPSum
          if (normalisedIPFrom * fSupply > fixedCost) interactionPotential else 0.0
        } else 0.0
    }
  }

  type InteractionPotential = (Double, Double, Double) => Double

  def gravityPotential(distanceDecay: Double): InteractionPotential =
    (mass1: Double, mass2: Double, distance: Double) => (mass1 * mass2) / math.pow(distance, distanceDecay)

  def supplies(populations: Vector[Double], economicMultiplier: Double, activity: Activity) =
    populations.map(p => supply(p, economicMultiplier, activity))

  def demands(populations: Vector[Double], economicMultiplier: Double, activity: Activity) =
    populations.map(p => demand(p, economicMultiplier, activity))

  def demand(population: Double, economicMultiplier: Double, activity: Activity) =
    economicMultiplier * activity.exchangeRate * math.pow(population, activity.sizeEffectOnDemand)

  def supply(population: Double, economicMultiplier: Double, activity: Activity) =
    economicMultiplier * activity.exchangeRate * math.pow(population, activity.sizeEffectOnSupply)

}
