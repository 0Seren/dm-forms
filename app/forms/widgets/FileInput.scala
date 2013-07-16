package forms.widgets

import scala.xml._

class FileInput(
  required: Boolean,
  attrs: MetaData = Null) extends Widget(required, attrs) {
	def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
		<input type="file" name={ name } value={ if (value.isEmpty) "" else value(0) }></input> % attrs % reqAttr % attrList
  }
	override def needsMultipartForm: Boolean = true
}