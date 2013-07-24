package forms.fields

import forms.widgets._
import forms.validators.ValidationError

/*
 * radio field creates a radio field that returns choices._2 when the user selects the radio field
 * with choices._1 displayed next to it.
 */

/**
 * Contains methods used in RadioField. There is no RadioFieldOptional as users can't deselect
 * an option in a radio field. Instead, try using ChoiceFieldOptional for RadioFieldOptional.
 */
abstract class BaseRadioField[T, U](name: String, choices: List[(String, T)])(implicit man: Manifest[U]) extends Field[U](name) {
  
  /**
   * Sets the widget for Radio Fields
   */
  override def widget = new RadioInput(required, choices.map(_._1))
  
  /**
   * Turns the value: Option[U] into a list of string indexes, and returning List("-1")
   * if value is empty.
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
 * Creates a new required RadioField. There is no RadioFieldOptional as users can't deselect
 * an option in a radio field. Instead, try using ChoiceField for RadioFieldOptional.
 */
class RadioField[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) extends BaseRadioField[T, T](name, choices) {
  
  /**
   * Returns one value from choices._2 of type T. If none, more than one, or some other error occurs,
   * a ValidationError is returned.
   */
  def asValue(s: Seq[String]): Either[ValidationError, T] = {
    try {
    	if(s.isEmpty || s.contains("-1")) Left(ValidationError("This Field Is Required."))
    	else if (s.contains("-2")) Left(ValidationError("Well, this is embarising... We're not really sure what just happened. Please try again."))
    	else if(s.size > 1) Left(ValidationError("Can Only Select One Value"))
    	else Right(choices(s(0).toInt)._2)
    } catch {
      	case i: IndexOutOfBoundsException => Left(ValidationError("IndexOutOfBoundsException"))
      	case n: NumberFormatException => Left(ValidationError("NumberFomatException"))
    	case e: Throwable => Left(ValidationError("Invalid Input: "+e.toString))
    }
  }
}