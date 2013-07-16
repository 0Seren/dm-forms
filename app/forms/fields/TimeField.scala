package forms.fields

import scala.reflect.runtime.universe.TypeTag

import org.joda.time.LocalTime
import org.joda.time.format.{ DateTimeFormat, DateTimeFormatter, DateTimeFormatterBuilder }

import forms.validators.ValidationError
import forms.widgets.TimeInput

abstract class BaseTimeField[T](name: String, parser: DateTimeFormatter = BaseTimeField.defaultParser)(implicit tag: TypeTag[T])
    extends Field[T](name) {  
  override def widget = new TimeInput(required)
}

object BaseTimeField {
  // require hours, minutes, and either am/AM or pm/PM; seconds and a space before AM/PM are optional
  val defaultFormats = Array("h:mma", "h:mm a", "h:mm:ssa", "h:mm:ss a").map(x=> DateTimeFormat.forPattern(x).getParser)
  val defaultParser: DateTimeFormatter = new DateTimeFormatterBuilder().append(null, defaultFormats).toFormatter
  
  def asValue(time: String, parser: DateTimeFormatter): Either[ValidationError, LocalTime] = {
    try {
      Right(parser.parseLocalTime(time))
    } catch {
      // TODO: error message should be based on formats provided
      case e: IllegalArgumentException => Left(ValidationError(s"Check your time format; '12:30 PM' is an example of a correct time."))
    } 
  }
}

class TimeField(name: String, parser: DateTimeFormatter = BaseTimeField.defaultParser) extends BaseTimeField[LocalTime](name, parser) {
 
  def asValue(s: Seq[String]): Either[ValidationError, LocalTime] = BaseTimeField.asValue(s(0), parser)
}

class TimeFieldOptional(name: String, parser: DateTimeFormatter = BaseTimeField.defaultParser) extends BaseTimeField[Option[LocalTime]](name) {
  override def required = false
  
  def asValue(s: Seq[String]): Either[ValidationError, Option[LocalTime]] = {
    if (s.isEmpty) Right(None)
    else BaseTimeField.asValue(s(0), parser).fold(Left(_), (lt: LocalTime) => Right(Some(lt)))
  }
}