package forms.widgets

import scala.xml._
import forms.validators.ValidationError

/**
 * A widget that creates a generic input field.
 */
abstract class Input(
    required : Boolean,
    attrs: MetaData = Null, step: Boolean = false) extends Widget(required, attrs) {

  /**
   * Sets the input type of the field.
   */
  def inputType: String
  
  /**
   * Renders the input using xml.
   */
  def render(name: String, value: Seq[String], attrList: MetaData): NodeSeq = {
    val valueAttr = value match {
      case Seq(s) => new UnprefixedAttribute("value", Text(s), Null)
      // fails silently if we get too many values for a single-valued field
      case _ => Null
    }
    if(!step) <input type={inputType} name={ name } /> % attrs % reqAttr % attrList % valueAttr
    else <input type={inputType} step="any" name={ name } /> % attrs % reqAttr % attrList % valueAttr
  }
}