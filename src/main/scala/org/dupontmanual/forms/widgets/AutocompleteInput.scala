package forms.widgets

import scala.xml._
import scalatags._
import forms.validators.ValidationError

/**
 * A Widget that uses the Bootstrap typeahead javascript.
 */
class AutocompleteInput(
  required: Boolean,
  array: String = "",
  attrs: MetaData = Null) extends Widget(required, attrs) {

  /**
   * Renders the field using xml.
   */
  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    val valueAttr = if (value.isEmpty) "" else value(0)
    input.ctype("text").name(name).cls("ac").value(valueAttr).attr(attrList.asAttrMap.toList :_*).attr(("data-provide","typeahead"), ("data-source",array), ("data-items", "12"), ("autocomplete", "off")).toXML
  }
}