package forms.fields
import forms.validators._
import forms.widgets.TextInput

/**
 * Sets methods for both EmailField and EmailFieldOptional
 */
trait BaseEmailField[T] extends Field[T] {
  override def widget = new TextInput(required, _inputType="email")
}

/**
 * Creates a required EmailField.
 */
class EmailField(name: String) extends TextField(name) with BaseEmailField[String] {
  
  /**
   * Adds an email validator to the default validators.
   */
  override def validators = super.validators ++ List(EmailValidator)
  
  /**
   * Returns a ValidationError if empty or a string of the user's input.
   */
  override def asValue(strs: Seq[String]) = super.asValue(strs.map(_.trim))
}

/**
 * Creates an optional email field.
 */
class EmailFieldOptional(name: String) extends TextFieldOptional(name) with BaseEmailField[Option[String]] {
  
  /**
   * Adds the option email validator the the default validators.
   */
  override def validators = super.validators ++ OptionValidator(List(EmailValidator))
  
  /**
   * Returns either a ValidationError or the user's input as a string.
   */
  override def asValue(strs: Seq[String]) = super.asValue(strs.map(_.trim))
}

