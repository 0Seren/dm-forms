package forms.widgets

import scala.xml._

class TextInput(
    required : Boolean,
    attrs: MetaData = Null, _inputType: String = "text", _step: Boolean = false) extends Input(required, attrs, step=_step) {
  
  def inputType: String = _inputType
}
