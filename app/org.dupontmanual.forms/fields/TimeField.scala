package forms.fields

import scala.reflect.runtime.universe.TypeTag

import org.joda.time.LocalTime
import org.joda.time.format.{ DateTimeFormat, DateTimeFormatter, DateTimeFormatterBuilder }

import forms.validators.ValidationError
import forms.widgets.TimeInput

/*
 * timeField will return either a Validation Error or a org.joda.time.LocalTime
 */

/**
 * Creates methods common to both TimeField and TimeFieldOptional.
 */
abstract class BaseTimeField[T](name: String, parser: DateTimeFormatter = BaseTimeField.defaultParser)(implicit tag: TypeTag[T])
    extends Field[T](name) {  
  
  /**
   * Sets the widget for TimeField and TimeFieldOptional.
   */
  override def widget = new TimeInput(required)
}

/**
 * Contains methods for the Time parser.
 */
object BaseTimeField {
  // require hours, minutes, and either am/AM or pm/PM; seconds and a space before AM/PM are optional
  
  /**
   * Creates the valid formats.
   */
  val defaultFormats = Array("h:mma", "h:mm a", "h:mm:ssa", "h:mm:ss a").map(x=> DateTimeFormat.forPattern(x).getParser)
  
  /**
   * Creates a parser from the default formats.
   */
  val defaultParser: DateTimeFormatter = new DateTimeFormatterBuilder().append(null, defaultFormats).toFormatter
  
  /**
   * Parses the time, and returns the result. Either LocalTime or a ValidationError.
   */
  def asValue(time: String, parser: DateTimeFormatter): Either[ValidationError, LocalTime] = {
    try {
      Right(parser.parseLocalTime(time))
    } catch {
      // TODO: error message should be based on formats provided
      case e: IllegalArgumentException => Left(ValidationError(s"Check your time format; '12:30 PM' is an example of a correct time."))
    } 
  }
}

/**
 * Creates a new required TimeField.
 */
class TimeField(name: String, parser: DateTimeFormatter = BaseTimeField.defaultParser) extends BaseTimeField[LocalTime](name, parser) {
 
  /**
   * Parses the value and returns a ValidationError if there are any errors with the input, and
   * LocalTime if there are no errors.
   */
  def asValue(s: Seq[String]): Either[ValidationError, LocalTime] = BaseTimeField.asValue(s(0), parser)
}

/**
 * Creates a new optional TimeField.
 */
class TimeFieldOptional(name: String, parser: DateTimeFormatter = BaseTimeField.defaultParser) extends BaseTimeField[Option[LocalTime]](name) {
  
  /**
   * Parses the time, and returns a ValidationError if it's not a legal format, and returns an
   * Option[LocalTime] if it is in a valid format or empty.
   */
  def asValue(s: Seq[String]): Either[ValidationError, Option[LocalTime]] = {
    if (s.isEmpty) Right(None)
    else BaseTimeField.asValue(s(0), parser).fold(Left(_), (lt: LocalTime) => Right(Some(lt)))
  }
}