package forms.widgets

import scala.xml._

/**
 * A widget that creats a File Field.
 */
class FileInput(
  required: Boolean,
  attrs: MetaData = Null) extends Widget(required, attrs) {
  
  /**
   * Renders the file field using xml.
   */
	def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
		<input type="file" name={ name } value={ if (value.isEmpty) "" else value(0) }></input> % attrs % reqAttr % attrList
  }
	
	/**
	 * Sets needsMultipartForm to true so that the Form knows it needs to have an enctype of multipart form data.
	 */
	override def needsMultipartForm: Boolean = true
}