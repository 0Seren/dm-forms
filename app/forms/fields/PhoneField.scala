package forms.fields

import forms.validators.ValidationError
import forms.widgets.PhoneInput
import forms._

abstract class BasePhoneField[T](name: String)(implicit man: Manifest[T]) extends Field[T](name) {

  override def widget = new PhoneInput(required)

  def isAllDigits(s: String) = s forall Character.isDigit
}

class PhoneField(name: String) extends BasePhoneField[String](name) {

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

class PhoneFieldOptional(name: String) extends BasePhoneField[Option[String]](name) {
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