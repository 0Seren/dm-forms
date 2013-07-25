package forms.widgets

import scala.xml._

/**
 * A widget of type Input that creates a hidden field.
 */
class HiddenInput(
    required: Boolean,
    attrs: MetaData = Null) extends Input(required, attrs) {
  
  /**
   * Sets isHidden to true.
   */
  override def isHidden: Boolean = true
  
  /**
   * Sets the input type to hidden.
   */
  def inputType: String = "hidden"
}
