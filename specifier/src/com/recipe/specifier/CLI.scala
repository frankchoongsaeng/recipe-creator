package com.recipe.specifier

import com.recipe.specifier.recipe.{Action, Addition, Entry}

import java.io.{File, PrintWriter}
import scala.io.StdIn
import scala.util.{Failure, Success}

object CLI extends App {
  var recipe: RecipeCreator.Recipe = RecipeCreator.blank

  val availableCommands: String = {
    String.join("\n",
      "\tAdd - usage: \"add: (ingredient) (quantity)\"",
      "\tExample",
      "\t\tadd: salt 2tbsp",
      "\t\tadd: vegetable oil 2tbsp",
      "\n",
      "\tDo - usage: \"do: (action)\"",
      "\tExample",
      "\t\tdo: pour some water into a pot",
      "\t\tdo: chop plantain to reasonable size",
    )
  }

  def save(_recipe: RecipeCreator.Recipe): Unit = {
    try {
      val fileContent = upickle.default.write[RecipeCreator.Recipe](_recipe, 4)
      val fileObject = new File("recipe.json")
      val printWriter = new PrintWriter(fileObject)
      printWriter.write(fileContent)
      printWriter.close()
      println("Recipe saved successfully")
    } catch {
      case e: Exception =>
        println(s"Could not save the recipe to disk.\n${e.printStackTrace()}")

    }
  }

  while (true) {
    print("Enter a command: ")
    val input = StdIn.readLine()

    Command.fromString(input) match {
      case Failure(exception) =>
        println(s"could not process command. Reason: ${exception.getMessage}")
        println("Here are a list of available commands:\n")
        println(availableCommands)

      case Success(command) =>
        command match {
          case Add(ingredient, quantity) =>
            recipe = RecipeCreator.entry(Addition(ingredient, quantity), recipe)
            save(recipe)

          case Do(action) =>
            recipe = RecipeCreator.entry(Action(action), recipe)
            save(recipe)

          case Help() =>
            println("Here are a list of available commands:\n")
            println(availableCommands)
        }
    }
  }
  def emptyList: List[Int] = List()

  def makeList(el:Int, list: List[Int]) = ???

  def listCreator(items: List[Int]) : List[Int] = {
    items match {
      case _ :: List() =>
        emptyList

      case first :: rest =>
        makeList(first, listCreator(rest))
    }
  }
}
