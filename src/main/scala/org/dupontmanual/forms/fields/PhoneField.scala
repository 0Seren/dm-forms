package org.dupontmanual.forms.fields


import org.dupontmanual.forms.validators.ValidationError
import org.dupontmanual.forms.widgets.PhoneInput
import org.dupontmanual.forms._

/**
 * Creates methods common to both PhoneField and PhoneFieldOptional.
 */
abstract class BasePhoneField[T](name: String)(implicit man: Manifest[T]) extends Field[T](name) {
  
  /**
   * Determines if the masked input javascript should be used.
   */
  def useMaskedInputs: Boolean = true
  
  /**
   * Sets the place holder for the masked input javascript.
   */
  def placeHolder: Char = '_'
    
  /**
   * Sets the format for the phone input. '#' means that a number, 0-9, should be in that spot, and any other characters mean that
   * said character should be in that spot. For instance, "###-###-####" means that "123-456-7890" is a valid input.
   */
  def phoneFormat: String = "###-###-####"
  
  /**
   * Sets the widget for PhoneField and PhoneFieldOptional.
   */
  override def widget = new PhoneInput(required, useMaskedInputs=useMaskedInputs, placeHolder=placeHolder, phoneFormat = phoneFormat)

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
   * Makes sure the input is in `phoneFormat` format. Returns their input
   * if it is in the valid format, and a ValidationError if the field is empty
   * or if it is in the invalid format.
   */
  def asValue(s: Seq[String]): Either[ValidationError, String] = {
    if(s.isEmpty) Left(ValidationError("This Field is required"))
    else if(phoneFormat.length != s(0).length) Left(ValidationError("Make sure input is in "+phoneFormat+" format."))
    else{
      if(s(0).zipWithIndex.map((ci) => 
        if(phoneFormat.charAt(ci._2) == '#') ci._1.isDigit
        else ci._1 == phoneFormat.charAt(ci._2) 
      ).contains(false)) Left(ValidationError("Make sure input is in "+phoneFormat+" format."))
      else Right(s(0))
    }
  }
}

/**
 * Creates a new optional phone field.
 */
class PhoneFieldOptional(name: String) extends BasePhoneField[Option[String]](name) {
  
  /**
   * Makes sure the input is in `phoneFormat` format. If not, then it returns a ValidationError,
   * otherwise it returns an Option[String] of the user's input.
   */
    def asValue(s: Seq[String]): Either[ValidationError, Option[String]] = {
    if(s.isEmpty) Right(None)
    else if(phoneFormat.length != s(0).length) Left(ValidationError("Make sure input is in "+phoneFormat+" format."))
    else{
      if(s(0).zipWithIndex.map((ci) => 
        if(phoneFormat.charAt(ci._2) == '#') ci._1.isDigit
        else ci._1 == phoneFormat.charAt(ci._2) 
      ).contains(false)) Left(ValidationError("Make sure input is in "+phoneFormat+" format."))
      else Right(Some(s(0)))
    }
  }
}
