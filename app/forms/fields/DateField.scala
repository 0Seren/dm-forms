package forms.fields

import scala.reflect.runtime.universe._

import org.joda.time.LocalDate
import org.joda.time.format.{ DateTimeFormat, DateTimeFormatter }

import forms.validators.ValidationError
import forms.widgets.DateInput

abstract class BaseDateField[T](name: String, parser: DateTimeFormatter = BaseDateField.usFormat)(implicit tag: TypeTag[T]) extends Field[T](name) {
  
  override def widget = new DateInput(required)
}

object BaseDateField {
  val usFormat = DateTimeFormat.forPattern("MM/dd/YYYY")

  def asValue(date: String, parser: DateTimeFormatter): Either[ValidationError, LocalDate] = {
    try {
	  Right(parser.parseLocalDate(date))
    } catch {
      case e: IllegalArgumentException => Left(ValidationError(s"Check your date format; a correct entry for today would be '${BaseDateField.usFormat.print(LocalDate.now())}'."))
    }
  }
}

class DateField(name: String, parser: DateTimeFormatter = BaseDateField.usFormat) extends BaseDateField[LocalDate](name, parser) {
  def asValue(inputs: Seq[String]): Either[ValidationError, LocalDate] = BaseDateField.asValue(inputs(0), parser)
}

class DateFieldOptional(name: String, parser: DateTimeFormatter = BaseDateField.usFormat) extends BaseDateField[Option[LocalDate]](name, parser) {
  def asValue(inputs: Seq[String]): Either[ValidationError, Option[LocalDate]] = {
    if (inputs.isEmpty) Right(None)
    else BaseDateField.asValue(inputs(0), parser).fold((ve: ValidationError) => Left(ve), (date: LocalDate) => Right(Some(date)))
  }
}