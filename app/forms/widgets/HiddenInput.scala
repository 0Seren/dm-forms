package forms.widgets

import scala.xml._

class HiddenInput(
    required: Boolean,
    attrs: MetaData = Null) extends Input(required, attrs) {
  
  override def isHidden: Boolean = true
  
  def inputType: String = "hidden"
}
