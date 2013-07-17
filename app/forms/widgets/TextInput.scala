package forms.widgets

import scala.xml._

/**
 * A widget that creates a text field.
 */
class TextInput(
    required : Boolean,
    attrs: MetaData = Null, _inputType: String = "text", _step: Boolean = false) extends Input(required, attrs, step=_step) {
  
  /**
   * Sets the input type to be _inputType.
   */
  def inputType: String = _inputType
}
