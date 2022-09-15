package com.recipe.specifier

import com.recipe.specifier.recipe.Measurement
import scala.util.{Failure, Success, Try}

/**
 * An abstract command class
 */
abstract class Command

/**
 * Add command represents an addition into the recipe.
 *
 * @param ingredient The ingredient to be added. Multiple word ingredients like 'sea salt' can be used as well.
 * @param quantity   The quantity (in decimal, fraction, or whole number) of the ingredient, along with the measurement unit like 2tbsp or 1cup
 */
final case class Add(ingredient: String, quantity: recipe.Measurement) extends Command

final case class Do(action: String) extends Command

final case class Help() extends Command

object Command {
  def fromString(command: String): Try[Command] = {
    command.split("\\s").toList match {
      case add :: args if add.toLowerCase == "add:" =>
        args match {
          case ingr :: qty :: List() =>
            Measurement(qty) match {
              case Some(measurement: recipe.Measurement) =>
                Success(Add(ingr, measurement))
              case None =>
                Failure(new Throwable(s"Could not parse quantity: $qty"))
            }

          case more if more.length > 2 =>
            Measurement(more.last) match {
              case Some(measurement: recipe.Measurement) =>
                Success(Add(more.reverse.tail.reverse.mkString(" "), measurement))
              case None =>
                Failure(new Throwable(s"Could not parse quantity: ${more.last}"))
            }

          case _ =>
            Failure(new Throwable("Add command has too few arguments"))
        }

      case action :: rest if action.toLowerCase == "do:" =>
        Success(Do(rest mkString " "))

      case help :: List() if help == "help" =>
        Success(Help())

      case other =>
        Failure(new Throwable(s"Unknown command '${other.mkString(" ")}'"))
    }
  }
}