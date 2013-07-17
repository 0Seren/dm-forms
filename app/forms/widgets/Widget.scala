package forms.widgets

import scala.language.implicitConversions

import scala.xml._
import scala.xml.UnprefixedAttribute
import play.api.mvc.MultipartFormData.FilePart

/**
 * A Widget represents the Html into which the input values
 * would be entered by the user. The method that needs to be
 * overridden by most classes would be render, which should
 * produce a NodeSeq with input elements
 */
abstract class Widget(
    val required: Boolean,
    val attrs: MetaData = Null) {

  /**
   * A value that sets the field to be hidden or not.
   */
  def isHidden: Boolean = false

  /**
   * A value that sets whether the field needs multipart form data. Only true for FileInput.
   */
  def needsMultipartForm: Boolean = false
  
  /**
   * Scripts that are used on that field.
   */
  def scripts: NodeSeq = NodeSeq.Empty

  /**
   * How the field is rendered or shown on the webpage. Uses xml.
   */
  def render(name: String, value: Seq[String], attrList: MetaData = Null): NodeSeq
  
  //TODO: need something different for files
  /**
   * Gets the value from Datadict.
   */
  def valueFromDatadict(data: Map[String, Seq[String]], name: String): Seq[String] = {
    data.getOrElse(name, Nil)
  }
  
  /**
   * Creates a required attribute if needed.
   */
  def reqAttr: MetaData = if (required) new UnprefixedAttribute("required", Text("required"), Null) else Null

}

/**
 * Contains helpful methods for the widget.
 */
object Widget {
  
  /**
   * Takes in a Map[String, String] and turns in into MetaData.
   */
  implicit def map2MetaData(attrs: Map[String, String]): MetaData = {
    attrs.foldRight[MetaData](Null)(
	  (keyValue: (String, String), rest: MetaData) => 
	    new UnprefixedAttribute(keyValue._1, keyValue._2, rest))
  }
}
