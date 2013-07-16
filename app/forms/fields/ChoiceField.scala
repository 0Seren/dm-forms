package forms.fields

import forms.widgets.SelectInput
import forms.validators.ValidationError

abstract class BaseChoiceField[T, U](name: String, choices: List[(String, T)])(implicit man: Manifest[U]) extends Field[U](name) {
  override def widget = new SelectInput(required, choices.map(_._1))
}

class ChoiceField[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) extends BaseChoiceField[T, T](name, choices) {
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

  override def asStringSeq(value: Option[T]): Seq[String] = value match {
    case None => List("-1")
    case Some(t) => List(choices.map(_._2).indexOf(t).toString)
  }
}

class ChoiceFieldOptional[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) extends BaseChoiceField[T, Option[T]](name, choices) {
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
  override def asStringSeq(value: Option[Option[T]]): Seq[String] = value match {
    case None => List("-1")
    case Some(t) => List(choices.map(_._2).indexOf(t).toString)
  }
}

class ChoiceFieldMultiple[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) extends BaseChoiceField[T, List[T]](name, choices) {
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
      case _ => Left(ValidationError("Expected a single value, got none or multiple."))
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