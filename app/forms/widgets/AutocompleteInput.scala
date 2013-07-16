package forms.widgets

import scala.xml._
import scalatags._
import forms.validators.ValidationError

class AutocompleteInput(
  required: Boolean,
  array: String = "",
  uuid: java.util.UUID,
  attrs: MetaData = Null) extends Widget(required, attrs) {

  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    val valueAttr = if (value.isEmpty) "" else value(0)
    input.ctype("text").name(name).cls("ac").value(valueAttr).attr(attrList.asAttrMap.toList :_*).attr(("data-provide","typeahead"), ("data-source",array), ("autocomplete", "off")).toXML
  }
}