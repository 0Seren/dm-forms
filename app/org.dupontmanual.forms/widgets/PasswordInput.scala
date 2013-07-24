package forms.widgets

import scala.xml._

/**
 * A widget that creates a password field.
 */
class PasswordInput(
    required: Boolean,
    attrs: MetaData = Null,
    val renderValue: Boolean = false) extends Input(required, attrs) {

  /**
   * Sets the input type to be password.
   */
  def inputType: String = "password"
    
  /**
   * Renders the field using xml and deleting the value if renderValue is false.
   */  
  override def render(name: String, value: Seq[String], attrList: MetaData) = {
    super.render(name, if (renderValue) value else Nil, attrList)
  }
}