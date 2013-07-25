package forms.fields

import forms.widgets.SelectInput
import forms.validators.ValidationError

/**
 * Sets the widget for ChoiceField and ChoiceFieldOptional
 */
abstract class BaseChoiceField[T, U](name: String, choices: List[(String, T)])(implicit man: Manifest[U]) extends Field[U](name) {
  override def widget = new SelectInput(required, choices.map(_._1))
}

/**
 * ChoiceField returns the user's selection from a dropdown field. choices._1 is what is displayed to the user,
 * and choices._2 is what is returned should the user select that option.
 */
class ChoiceField[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) extends BaseChoiceField[T, T](name, choices) {
  
  /**
   * Returns a ValidationError if notthing was selected, or returns the T coresponding to the selected value.
   */
  def asValue(s: Seq[String]): Either[ValidationError, T] = {
    s match {
      case Seq(s) => try {
        val index = s.toInt
        if (index == -1) Left(ValidationError("This field is required. Please choose a value."))
        else if (index >= choices.length) Left(ValidationError("Illegal value submitted."))
        else Right(choices(index)._2)
      } catch {
        case e: NumberFormatException => Left(ValidationError("Illegal value submitted."))
      }
      case _ => Left(ValidationError("Expected a single value, got none or multiple."))
    }
  }

  /**
   * Returns the correct index as a string for the user's selection, and -1 if none.
   */
  override def asStringSeq(value: Option[T]): Seq[String] = value match {
    case None => List("-1")
    case Some(t) => List(choices.map(_._2).indexOf(t).toString)
  }
}

/**
 * ChoiceFieldOptional returns the user's selection from a dropdown field. choices._1 is what is displayed to the user,
 * and choices._2 is what is returned should the user select that option.
 */
class ChoiceFieldOptional[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) extends BaseChoiceField[T, Option[T]](name, choices) {
  
  /**
   * Returns None if nothing is selected or Option[T] if something was selected.
   */
  def asValue(s: Seq[String]): Either[ValidationError, Option[T]] = { //i made the T become Option[T] so as to enable Right(None) 
    s match {
      case Seq(s) => try {
        val index = s.toInt
        if (index < -1 || index > choices.length - 1) {
          Left(ValidationError("Illegal value submitted."))
        } else {
          if (index == -1) {
            Right(None)
          } else {
            Right(Some(choices(index)._2))
          }
        }
      } catch {
        case e: NumberFormatException => Left(ValidationError("Illegal value submitted."))
      }
      case _ => Left(ValidationError("Expected a single value, got none or multiple."))
    }
  }
  
  /**
   * Returns the correct index as a string for the user's selection, and -1 if none.
   */
  override def asStringSeq(value: Option[Option[T]]): Seq[String] = value match {
    case None => List("-1")
    case Some(t) => List(choices.map(_._2).indexOf(t).toString)
  }
}

/**
 * Allows the user to select multiple choices from a dropdown list. Is required.
 */
class ChoiceFieldMultiple[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) extends BaseChoiceField[T, List[T]](name, choices) {
  
  /**
   * Returns a ValidationError if none, or a List[T] crresponding to the user's selections.
   */
  def asValue(s: Seq[String]): Either[ValidationError, List[T]] = {
    s match {
      case Seq(s) => try {
        var listOfIndexes = List[Int]()
        for (xs <- s) {
          listOfIndexes = (xs.toInt) :: listOfIndexes
        }
        var listOfT = List[T]()
        for (index <- listOfIndexes) {
          if (index < 0 || index > choices.length - 1) {
            Left(ValidationError("Illegal value submitted."))
          } else {
            listOfT = choices(index)._2 :: listOfT
          }
        }
        Right(listOfT)
      } catch {
        case e: NumberFormatException => Left(ValidationError("Illegal value submitted."))
      }
      case _ => Left(ValidationError("Expected one or more values, got none or something else."))
    }
  }
  override def asStringSeq(value: Option[List[T]]): Seq[String] = {
    value match {
      case None => List("-1")
      case Some(t) => {
        var listReturned = List[String]()
        for (value2 <- value) {
          listReturned = choices.map(_._2).indexOf(t).toString :: listReturned
        }
        listReturned
      }
    }
  }
}