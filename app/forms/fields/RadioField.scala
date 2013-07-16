package forms.fields

import forms.widgets._
import forms.validators.ValidationError

abstract class BaseRadioField[T, U](name: String, choices: List[(String, T)])(implicit man: Manifest[U]) extends Field[U](name) {
  override def widget = new RadioInput(required, choices.map(_._1))
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

class RadioField[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) extends BaseRadioField[T, T](name, choices) {
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