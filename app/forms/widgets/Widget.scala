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

  def isHidden: Boolean = false

  def needsMultipartForm: Boolean = false
  
  def scripts: NodeSeq = NodeSeq.Empty

  def render(name: String, value: Seq[String], attrList: MetaData = Null): NodeSeq
  
  //TODO: need something different for files
  def valueFromDatadict(data: Map[String, Seq[String]], name: String): Seq[String] = {
    data.getOrElse(name, Nil)
  }
  
  def reqAttr: MetaData = if (required) new UnprefixedAttribute("required", Text("required"), Null) else Null

}

object Widget {
  implicit def map2MetaData(attrs: Map[String, String]): MetaData = {
    attrs.foldRight[MetaData](Null)(
	  (keyValue: (String, String), rest: MetaData) => 
	    new UnprefixedAttribute(keyValue._1, keyValue._2, rest))
  }
}
