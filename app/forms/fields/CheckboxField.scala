package forms.fields

import forms.widgets._
import forms.validators.ValidationError

abstract class BaseCheckboxField[T, U](name: String, choices: List[(String, T)], useSelectInputMult: Boolean = false)(implicit man: Manifest[U]) extends Field[U](name) {
  override def widget = if(useSelectInputMult) new SelectInput(required, choices.map(_._1), allowMultiple = true)
	  					else new CheckboxInput(required, choices.map(_._1))
  override def asStringSeq(value: Option[U]): Seq[String] = {
    value match {
      case None => List("-1")
      case Some(t) => t match {
        case e: List[T] => {
        	if(e.isEmpty) List("-1")
        	else {
        		var listReturned = List[String]()
        		for (value2 <- e) {
        			listReturned = choices.map(_._2).indexOf(e).toString :: listReturned
        		}
        		listReturned
        	}
        }
        case _ => List("-2")
      }
    }
  }
}

class CheckboxField[T](name: String, choices: List[(String, T)], useSelectInputMult: Boolean = false)(implicit man: Manifest[T]) extends BaseCheckboxField[T, List[T]](name, choices, useSelectInputMult) {
  def asValue(s: Seq[String]): Either[ValidationError, List[T]] = {
    try {
    	val LOI = s.map(str => str.toInt) //LOI = listOfIndexes but I'm too lazy to rewrite
    	if(LOI.isEmpty || LOI.contains(-1)) Left(ValidationError("This field is required."))
    	else if (LOI.contains(-2)) Left(ValidationError("Well, this is embarising... We're not really sure what just happened. Please try again."))
    	else {
    	  Right(LOI.map(index => choices(index)._2).toList) //returns the objects
    	}
    } catch {
      	case i: IndexOutOfBoundsException => Left(ValidationError("IndexOutOfBoundsException"))
      	case n: NumberFormatException => Left(ValidationError("NumberFomatException"))
    	case e: Throwable => Left(ValidationError("Invalid Input: "+e.toString))
    }
  }
}

class CheckboxFieldOptional[T](name: String, choices: List[(String, T)], useSelectInputMult: Boolean = false)(implicit man: Manifest[T]) extends BaseCheckboxField[T, Option[List[T]]](name, choices, useSelectInputMult) {
  def asValue(s: Seq[String]): Either[ValidationError, Option[List[T]]] = {
    try {
    	val LOI = s.map(str => str.toInt) //LOI = listOfIndexes but I'm too lazy to rewrite
    	if(LOI.isEmpty || LOI.contains(-1)) Right(None)
    	else if (LOI.contains(-2)) Left(ValidationError("Well, this is embarising... We're not really sure what just happened. Please try again."))
    	else {
    	  Right(Some(LOI.map(index => choices(index)._2).toList))
    	}
    } catch {
      	case i: IndexOutOfBoundsException => Left(ValidationError("IndexOutOfBoundsException"))
      	case n: NumberFormatException => Left(ValidationError("NumberFomatException"))
    	case e: Throwable => Left(ValidationError("Invalid Input: "+e.toString))
    }
  }
}