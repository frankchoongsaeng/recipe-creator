package com.recipe.specifier

import com.recipe.specifier.recipe.{Action, Addition, Entry, Measurement}

import scala.language.implicitConversions

object RecipeCreator {
  type Recipe = List[Entry]

  /**
   * Type alias for ingredient.
   * The parts are the ingredients (name, (quantity, measurement unit))
   */
  type Ingredient = (String, (Float, String))

  def blank: Recipe = List[Entry]()

  def entry(entry: Entry, process: List[Entry]): Recipe = process :+ entry

  /**
   * Extracts a list of ingredients.
   * Combines ingredients having the same name into one by accumulating their quantity.
   *
   * @param recipe Recipe from which to extract ingredients
   * @return IngredientList
   */
  def ingredientsList(recipe: Recipe): List[Ingredient] = {
    val quantityByName: Map[String, (Float, String)] =
      recipe.foldLeft(Map[String, (Float, String)]())((map, entry) => {
        entry match {
          case Addition(ingredient, measurement) =>
            val measurementAsTsp: Float = Measurement.toTsp(measurement)
            val updatedPair: (Float, String) =
              map.get(ingredient)
                .map(pair =>
                  (pair._1 + measurementAsTsp, Measurement.compareMaxUnit(pair._2, measurement.value._2)))
                .getOrElse((measurementAsTsp, measurement.value._2))
            map +
              ((ingredient, updatedPair))

          case _ => map
        }
      })

    quantityByName.toList.map(ingr => {
      val (name, (qty, unit)) = ingr
      (name, (Measurement.convertFromTsp(qty, unit), unit))
    })
  }

  def ingredientsDoc(ingredientList: List[Ingredient]): String =
    ingredientList.foldLeft("")((strSoFar, ingredient) =>
      strSoFar ++
        s"${ingredient._1} - ${ingredient._2._1.toString} ${ingredient._2._2}\n"
    )

  def stepsDoc(recipe: Recipe): String = {
    recipe.zipWithIndex.map(zip => zip._1 match {
      case Action(action) =>
        (zip._2 + 1).toString ++ ". " ++ action.capitalize
      case Addition(ingredient, measurement) =>
        s"${(zip._2 + 1).toString}. Add - ${measurement.value._1} ${measurement.value._2} of $ingredient "
    }).mkString("\n")
  }

  def toDoc(recipe: RecipeCreator.Recipe): String = {
    val ingredientsSubDoc = ingredientsDoc(ingredientsList(recipe))
    val processSubDoc = stepsDoc(recipe)
    "INGREDIENTS\n" ++
      ingredientsSubDoc ++ "\n" ++
      "STEPS\n" ++
      processSubDoc
  }
}