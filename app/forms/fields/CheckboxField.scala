package forms.fields

import forms.widgets._
import forms.validators.ValidationError

/**
 * CheckboxField takes a name: String and choices: List[(String, T)]. The String in the tuples in choices
 * is what is displayed to the user, and T is any object you would like returned should the user select
 * that option. List[T] (if required), Option[List[T]] (if not required), or ValidationError will be returned.
 * Set the parameter useSelectInputMult to true if you wish to use the SelectInput widget.
 */
abstract class BaseCheckboxField[T, U](name: String, choices: List[(String, T)], useSelectInputMult: Boolean = false)(implicit man: Manifest[U]) extends Field[U](name) {
  
  /**
   * Sets the widget for both CheckboxField and CheckboxFieldOptional.
   */
  override def widget = if(useSelectInputMult) new SelectInput(required, choices.map(_._1), allowMultiple = true)
	  					else new CheckboxInput(required, choices.map(_._1))
  
  /**
   * Returns the correct indexes for the selected values.
   */
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

/**
 * A required set of checkboxes where at least one box has to be checked. name: String is the label for the
 * set of checkboxes, and choices: List[(String, T)] determines what is displayed by each checkbox (choices._1)
 * and what should be returned should that checkbox be checked (choices._2).
 */
class CheckboxField[T](name: String, choices: List[(String, T)], useSelectInputMult: Boolean = false)(implicit man: Manifest[T]) extends BaseCheckboxField[T, List[T]](name, choices, useSelectInputMult) {
  
  /**
   * Returns List(choices._2) or List[T] where the values in the list correspond to the
   * boxes checked by the user. Returns a ValidationError if something goes wrong or none were checked.  
   */
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

/**
 * An optional set of checkboxes. name: String is the label for the set of checkboxes, 
 * and choices: List[(String, T)] determines what is displayed by each checkbox (choices._1)
 * and what should be returned should that checkbox be checked (choices._2).
 */
class CheckboxFieldOptional[T](name: String, choices: List[(String, T)], useSelectInputMult: Boolean = false)(implicit man: Manifest[T]) extends BaseCheckboxField[T, Option[List[T]]](name, choices, useSelectInputMult) {
  
  /**
   * Returns None if no boxes are checked, and Some(List(choices._2)) or Option[List[T]] if some boxes are checked. 
   */
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