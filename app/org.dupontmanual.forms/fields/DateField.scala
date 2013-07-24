package forms.fields

import scala.reflect.runtime.universe._

import org.joda.time.LocalDate
import org.joda.time.format.{ DateTimeFormat, DateTimeFormatter }

import forms.validators.ValidationError
import forms.widgets.DateInput

/**
 * Contains methods shared by DateField and DateFieldOptioanal. 
 */
abstract class BaseDateField[T](name: String, parser: DateTimeFormatter = BaseDateField.usFormat)(implicit tag: TypeTag[T]) extends Field[T](name) {
  
  /**
   * Sets the widget for both DateField and DateFieldOptional
   */
  override def widget = new DateInput(required)
}

/**
 * Deals with the parsing of the input.
 */
object BaseDateField {
  
  /**
   * Determines the format for the parser
   */
  val usFormat = DateTimeFormat.forPattern("MM/dd/YYYY")

  /**
   * Creates a LocalDate or a ValidationError using the parser.
   */
  def asValue(date: String, parser: DateTimeFormatter): Either[ValidationError, LocalDate] = {
    try {
	  Right(parser.parseLocalDate(date))
    } catch {
      case e: IllegalArgumentException => Left(ValidationError(s"Check your date format; a correct entry for today would be '${BaseDateField.usFormat.print(LocalDate.now())}'."))
    }
  }
}

/**
 * DateField parses an input date and returns a LocalDate if the input is in the valid date format.
 */
class DateField(name: String, parser: DateTimeFormatter = BaseDateField.usFormat) extends BaseDateField[LocalDate](name, parser) {
  
  /**
   * Uses BaseDateField's parser to return either a ValidationError or a LocalDate.
   */
  def asValue(inputs: Seq[String]): Either[ValidationError, LocalDate] = BaseDateField.asValue(inputs(0), parser)
}

/**
 * Parses an input date and retuns an Option[LocalDate] if the input is in the valid date format.
 */
class DateFieldOptional(name: String, parser: DateTimeFormatter = BaseDateField.usFormat) extends BaseDateField[Option[LocalDate]](name, parser) {
  
  /**
   * Returns None if the field is empty, a ValidationError if the field isn't in the correct format, or Some(LocalDate)
   * if the input is in the correct fomat.
   */
  def asValue(inputs: Seq[String]): Either[ValidationError, Option[LocalDate]] = {
    if (inputs.isEmpty) Right(None)
    else BaseDateField.asValue(inputs(0), parser).fold((ve: ValidationError) => Left(ve), (date: LocalDate) => Right(Some(date)))
  }
}