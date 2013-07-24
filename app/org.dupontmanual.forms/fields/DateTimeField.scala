package forms.fields

import scala.reflect.runtime.universe.TypeTag

import org.joda.time.format.DateTimeFormatter
import org.joda.time.LocalDateTime

import forms.validators.ValidationError
import forms.widgets.DateTimeInput

/**
 * Sets common methods for DateTimeField and DateTimeFieldOptional.
 */
abstract class BaseDateTimeField[T](
    name: String, 
    dateParser: DateTimeFormatter = BaseDateField.usFormat, 
    timeParser: DateTimeFormatter = BaseTimeField.defaultParser)(implicit tag: TypeTag[T]) extends Field[T](name) {
  
  /**
   * Sets the widget for DateTimeField and DateTimeFieldOptional.
   */
  override def widget = new DateTimeInput(name, required)
  
  /**
   * Checks to see if the fields were filled in and Returns a ValidationError if there is an issue.
   */
  override def checkRequired(rawData: Seq[String]): Either[ValidationError, Seq[String]] = {
    rawData.map(_.trim).filter(_ != "") match {
      case Seq() => if(required) Left(ValidationError(errorMessages("required"))) else Right(Seq[String]())
      case Seq(tOrD) => Left(ValidationError("Both date and time are required."))
      case Seq(date, time, rest@_*) => Right(Seq(date, time))
    }
  }
}

object BaseDateTimeField {
  
  // s(0) is the date, s(1) is the time
  /**
   * Parses both the Date and the Time correctly.
   */
  def asValue(dateString: String, timeString: String, dateParser: DateTimeFormatter, timeParser: DateTimeFormatter): Either[ValidationError, LocalDateTime] = {
    try {
      val date = BaseDateField.asValue(dateString, dateParser)
      val time = BaseTimeField.asValue(timeString, timeParser)
      (date, time) match {
        case (Right(d), Right(t)) => Right(d.toLocalDateTime(t))
        case (Left(d), Right(t)) => Left(d)
        case (Right(d), Left(t)) => Left(t)
        case (Left(d), Left(t)) => Left(ValidationError(d.messages ++ t.messages))
      }
    } catch {
      case e: IllegalArgumentException => Left(ValidationError("Please make sure input is valid."))
    }
  }
}

/**
 * Creates a required DateTimeField.
 */
class DateTimeField(name: String, dateParser: DateTimeFormatter = BaseDateField.usFormat, 
    timeParser: DateTimeFormatter = BaseTimeField.defaultParser) extends BaseDateTimeField[LocalDateTime](name, dateParser, timeParser) {
  
  /**
   * Returns a ValidationError if empty or if improper format, ad returns LocalDateTime if fields are filled in correctly.
   */
  def asValue(s: Seq[String]): Either[ValidationError, LocalDateTime] = {
    BaseDateTimeField.asValue(s(0), s(1), dateParser, timeParser)
  }
}

/**
 * Creates an optional DateTimeField.
 */
class DateTimeFieldOptional(name: String, dateParser: DateTimeFormatter = BaseDateField.usFormat, 
    timeParser: DateTimeFormatter = BaseTimeField.defaultParser) extends BaseDateTimeField[Option[LocalDateTime]](name, dateParser, timeParser) {
  
  /**
   * Returns None if no input, Option[LocalDateTime] if formatted correctly, and ValidationError if
   * only one thing is filled in or are in incorrect format.
   */
  def asValue(s: Seq[String]): Either[ValidationError, Option[LocalDateTime]] = {
    s match {
      case Seq() => Right(None)
      case Seq(dOrT) => Left(ValidationError("Either supply both date and time, or neither."))
      case Seq(d, t) => BaseDateTimeField.asValue(d, t, dateParser, timeParser).fold(Left(_), (ldt: LocalDateTime) => Right(Some(ldt)))
    }
  }
}