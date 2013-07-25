package forms.widgets

import scala.xml._

// TODO: have switch for TinyMCE (or other) editor
/**
 * A widget that creates a text area field.
 */
class Textarea(required: Boolean, attrMap: MetaData = Null) 
extends Widget(
    required, 
    new UnprefixedAttribute("cols", Text("80"), 
    new UnprefixedAttribute("rows", Text("5"), Null).append(attrMap))) {

  /**
   * Renders the text area field using xml.
   */
  def render(name: String, value: Seq[String], attrList: MetaData = Null): NodeSeq = {
    //val tinymce = if (tinyMCE) new UnprefixedAttribute("class", Text("tinymce"), Null) else Null
    <textarea name={ name }>{ if (value.isEmpty) "" else value(0) }</textarea> % attrs.append(attrList)// % tinymce
  }
  
  /*override def scripts = if (tinyMCE) {
    <script src={ routes.Assets.at("javascripts/tinymce/jscripts/tiny_mce/tiny_mce.js").toString } type="text/javascript"></script> ++
    <script src={ routes.Assets.at("javascripts/tinymce/jscripts/tiny_mce/jquery.tinymce.js").toString } type="text/javascript"></script>
  } else NodeSeq.Empty */

}