package forms.widgets

import scala.xml._

class PasswordInput(
    required: Boolean,
    attrs: MetaData = Null,
    val renderValue: Boolean = false) extends Input(required, attrs) {

  def inputType: String = "password"
    
  override def render(name: String, value: Seq[String], attrList: MetaData) = {
    super.render(name, if (renderValue) value else Nil, attrList)
  }
}