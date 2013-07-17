package forms.fields
import forms.validators.ValidationError
import forms.widgets.CheckboxInput

/**
 * BooleanField takes a name or label for the field, and a string that will appear to the right
 * of a single checkbox for the user to select. It will return true if the checkbox is checked
 * or false if not checked.
 */
class BooleanField(name: String, text: String = "True") extends Field[Boolean](name) {
  
  /**
   * Sets required to false so checkbox can either be checked or not.
   */
  override def required = false
  
  /**
   * Sets the widget for the BooleanField.
   */
  override def widget = new CheckboxInput(required, List(text))
  
  /**
   * Returns false if not checked, and true if checked.
   */
  def asValue(s: Seq[String]): Either[ValidationError, Boolean] = {
    s match{
      case Seq() => Right(false)
      case Seq(str) => if(str == "0") Right(true) else if(str == "") Right(false) else Left(ValidationError("Boolean Fields need to be either selected or not."))
      case _ => Left(ValidationError("Boolean Fields need to be either selected or not."))
    }}
}