package com.recipe.specifier.service

import upickle.default._
import cask.endpoints.JsonData
import com.recipe.specifier.recipe.Addition
import com.recipe.specifier.recipe.Action
import com.recipe.specifier.{Add, Command, Do, RecipeCreator}

import java.io.{File, PrintWriter}
import scala.util.{Failure, Success}

case class ResponseStruct[A](error: Boolean, data: A)


object api extends cask.MainRoutes {
  @cask.get("/post")
  def hello(): String = {
    "Hello World!"
  }

  @cask.post("/do-thing")
  def doThing(request: cask.Request): String = {
    new String(request.readAllBytes()).reverse
  }

  @cask.postJson("api/recipe/update")
  def parse(command: ujson.Str, recipe: ujson.Arr): cask.Response[JsonData] = {
    Command fromString command.value match {
      case Failure(exception) =>
        cask.Response(
          ujson.Obj(
            "error" -> true,
            "data" -> ujson.Null,
            "message" -> s"could not parse command: ${exception.getMessage}"
          ), 400)

      case Success(command: Command) =>
        val deserializedRecipe: RecipeCreator.Recipe = read[RecipeCreator.Recipe](recipe)

        command match {
          case Add(ingredient, quantity) =>
            RecipeCreator.entry(Addition(ingredient, quantity), deserializedRecipe)

          case Do(action) =>
            RecipeCreator.entry(Action(action), deserializedRecipe)

          case _ =>
            ujson.Obj(
              "error" -> true,
              "data" -> ujson.Null,
              "message" -> s"unknown error occurred"
            )
        }
    }
  }

  @cask.postJson("/api/recipe/make-doc")
  def makeDoc(recipe: ujson.Arr) = {
    val recipeDoc = RecipeCreator.toDoc(read[RecipeCreator.Recipe](recipe))
    val file = new File("recipe.txt")
    val pw = new PrintWriter(file)
    pw.write(recipeDoc)
    pw.close()
//    scala.io.Source.fromFile("recipe.txt")
    recipeDoc
  }


  initialize()
}
