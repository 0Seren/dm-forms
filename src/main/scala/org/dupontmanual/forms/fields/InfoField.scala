package org.dupontmanual.forms.fields

import org.dupontmanual.forms.widgets.Widget
import scala.xml.{ Attribute, Null, Text }

class InfoField(name: String, value: String) extends TextField(name) {
  override def required = false
  override def initialVal = Some(value)
  override def widgetAttrs(widget: Widget) = {
    super.widgetAttrs(widget).append(Attribute("disabled", Text("disabled"), Null))
  }
  // disabled fields aren't sent in POST requests, so need to get data from 
  // initial values
  override def boundData(data: Seq[String], initial: Seq[String]) = initial
}