package com.recipe.specifier.recipe

import upickle.default.{ReadWriter => RW}
import upickle.default._

/**
 * An Entry into a recipe could either be an Action or Addition.
 * This way of modelling a recipe allows simplification with the ability to extract a listing
 * of all ingredients used in the recipe with their respective measurements.
 */
sealed trait Entry

object Entry {
  implicit val rw: RW[Entry] =
    RW.merge(
      Addition.rw,
      Action.rw
    )
}

/**
 * An action captures everything that could be done in the process of preparing a meal.
 * An example of an action would be "cutting onions", "marinate the chicken for 10 minutes",
 * etc.
 *
 * @param action The action to be performed in the process
 */
@upickle.implicits.key("action")
case class Action(action: String) extends Entry

object Action {
  implicit val rw: RW[Action] = macroRW[Action]
}

/**
 * Addition is how we capture the ingredients that go into the food.

 * @param ingredient the name of the ingredient
 * @param measurement quantity of the ingredient like 2tbsp, 8cup, 1tsp
 */
@upickle.implicits.key("addition")
case class Addition(ingredient: String, measurement: Measurement) extends Entry

object Addition {
  implicit val rw: RW[Addition] = macroRW[Addition]
}


