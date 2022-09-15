package com.recipe.specifier.recipe

import upickle.default.{ReadWriter => RW}
import upickle.default._

/**
 * A construct for capturing a measurement's quantity in a specific unit.
 */
sealed trait Measurement {
  def value: (Float, String)
}

object Measurement {
  val acceptableDigitFormat = "\\d+|\\d+\\.\\d+|\\d+/\\d+"

  def apply(string: String): Option[Measurement] =
    string match {
      case tbsp if tbsp.matches(s"($acceptableDigitFormat)tbsp") =>
        floatFromString(tbsp.replaceFirst("tbsp", ""))
          .map(Tbsp.apply)

      case tsp if tsp.matches(s"($acceptableDigitFormat)tsp") =>
        floatFromString(tsp.replaceFirst("tsp", ""))
          .map(Tsp.apply)

      case cup if cup.matches(s"($acceptableDigitFormat)cup") =>
        floatFromString(cup.replaceFirst("cup", ""))
          .map(Cup.apply)

      case _ =>
        None
    }

  def apply(qty: Float, unit: String): Option[Measurement] = {
    unit match {
      case "tbsp" => Some(Tbsp(qty))
      case "tsp" => Some(Tsp(qty))
      case "cup" => Some(Cup(qty))
      case _ => None
    }
  }

  def floatFromString(str: String): Option[Float] =
    str match {
      case fraction if fraction.matches("\\d+/\\d+") =>
        val parts = fraction.split("/").toList
        Some(parts.head.toFloat / parts(1).toFloat)

      case wholeNumber if wholeNumber.matches("\\d+") =>
        Some(wholeNumber.toFloat)

      case decimal if decimal.matches("\\d+.\\d+") =>
        Some(decimal.toFloat)

      case _ =>
        None

    }

  /**
   * Convert a Measurement to tsp
   *
   * @param measurement The measurement to convert to tsp
   * @return Tsp equivalent quantity
   */
  def toTsp(measurement: Measurement): Float = {
    measurement match {
      case Tbsp(quantity) =>
        quantity * 3.0f
      case Tsp(quantity) =>
        quantity
      case Cup(quantity) =>
        quantity * 48.692f
    }
  }

  /**
   * Convert from tsp to another unit
   *
   * @param qty  The quanity in tsp
   * @param unit The target unit of conversion
   * @return Quantity in the target unit
   */
  def convertFromTsp(qty: Float, unit: String): Float = {
    unit match {
      case "cup" => qty / 48.692f
      case "tbsp" => qty / 3
      case _ => qty
    }
  }

  def compareMaxUnit(u1: String, u2: String): String = {
    val defualt = 0
    val sizeOfUnit: Map[String, Int] = Map("tsp" -> 1, "tbsp" -> 2, "cup" -> 3)
    val max = math.max(sizeOfUnit.getOrElse(u1, defualt), sizeOfUnit.getOrElse(u2, defualt))
    sizeOfUnit.collectFirst({ case (unit, size) if size == max => unit }).getOrElse("")
  }

  // CODEC
  implicit val rw: RW[Measurement] =
    RW.merge(
      Tbsp.rw,
      Tsp.rw,
      Cup.rw
    )
}


@upickle.implicits.key("tbsp")
case class Tbsp(quantity: Float) extends Measurement {
  override def value: (Float, String) = (quantity, "tbsp")
}

object Tbsp {
  implicit val rw: RW[Tbsp] = macroRW[Tbsp]
}

@upickle.implicits.key("tsp")
case class Tsp(quantity: Float) extends Measurement {
  override def value: (Float, String) = (quantity, "tsp")
}

object Tsp {
  implicit val rw: RW[Tsp] = macroRW[Tsp]
}

@upickle.implicits.key("cup")
case class Cup(quantity: Float) extends Measurement {
  override def value: (Float, String) = (quantity, "cup")
}

object Cup {
  implicit val rw: RW[Cup] = macroRW[Cup]
}