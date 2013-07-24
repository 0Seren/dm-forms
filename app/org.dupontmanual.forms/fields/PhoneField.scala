package forms.fields

import forms.validators.ValidationError
import forms.widgets.PhoneInput
import forms._

/**
 * Creates methods common to both PhoneField and PhoneFieldOptional.
 */
abstract class BasePhoneField[T](name: String)(implicit man: Manifest[T]) extends Field[T](name) {

  /**
   * Sets the widget for PhoneField and PhoneFieldOptional.
   */
  override def widget = new PhoneInput(required)

  /**
   * Checks to see if all characters of a string are digits. 
   */
  def isAllDigits(s: String) = s forall Character.isDigit
}

/**
 * Creates a new required phone field.
 */
class PhoneField(name: String) extends BasePhoneField[String](name) {

  /**
   * Makes sure the input is in ###-###-#### form. Returns their input
   * if it is in the valid format, and a ValidationError if the field is empty
   * or if it is in the invalid format.
   */
  def asValue(s: Seq[String]): Either[ValidationError, String] = {
    val splitPhone = s(0).split("-")
    if (!(splitPhone.length == 3)) Left(ValidationError("Make sure input is in ###-###-#### format"))
    else if (!(splitPhone(0).length == 3 && splitPhone(1).length == 3 && splitPhone(2).length == 4)) {
      Left(ValidationError("Make sure input is in ###-###-#### format"))
    } else if (!(isAllDigits(splitPhone(0)) && isAllDigits(splitPhone(1)) && isAllDigits(splitPhone(2)))) {
      Left(ValidationError("Make sure input is in ###-###-#### format"))
    } else Right(s(0))
  }
}

/**
 * Creates a new optional phone field.
 */
class PhoneFieldOptional(name: String) extends BasePhoneField[Option[String]](name) {
  
  /**
   * Makes sure the input is in ###-###-#### form. If not, then it returns a ValidationError,
   * otherwise it returns an Option[String] of the user's input.
   */
  def asValue(s: Seq[String]): Either[ValidationError, Option[String]] = {
    try {
      val splitPhone = s(0).split("-")
      if (splitPhone.length != 3) Left(ValidationError("Make sure input is in ###-###-#### format"))
      else if (splitPhone(0).length != 3 || splitPhone(1).length != 3 || splitPhone(2).length != 4) {
        Left(ValidationError("Make sure input is in ###-###-#### format"))
      } else if (!(isAllDigits(splitPhone(0)) && isAllDigits(splitPhone(1)) && isAllDigits(splitPhone(2)))) {
        Left(ValidationError("Make sure input is in ###-###-#### format"))
      } else Right(Some(s(0)))
    } catch {
      case _: Exception => {
        if (s.isEmpty) Right(None)
        else Left(ValidationError("Make sure input is in ###-###-#### format"))
      }
    }
  }
}