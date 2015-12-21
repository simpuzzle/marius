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

import model._
import data._

object Test extends App {

  def fixedCost = 50
  def bonus = 50

  def model =
    marius(
      transactions = fixedCostTransactions(fixedCost),
      balance = bonusFixedCostsBalance(fixedCost, bonus) + unsolds - unsatisfieds,
      interactionPotential = gravityPotential(distanceDecay = 1.1),
      economicMultiplier = 0.589041240155966,
      wealthToPopulationExponent = 1.0,
      activities =
        Vector(
          Activity(
            sizeEffectOnDemand = 1.0841916528743,
            sizeEffectOnSupply = 1,
            exchangeRate = 1
          )
        )
    )

  val file = MariusFile(census = 0)
  val cities = file.cities(populationToWealthExponent = 1.02)

  def initialState = MariusState(
    step = 0,
    cities = cities,
    network = Network.full(cities.size),
    distanceMatrix = DistanceMatrix(file.positions))

  println(model.run(initialState))

}
