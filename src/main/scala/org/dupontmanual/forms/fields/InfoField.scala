package org.dupontmanual.forms.fields

import org.dupontmanual.forms.widgets.Widget
import scala.xml.{ Attribute, Null, Text }

class InfoField(name: String, value: String) extends TextField(name) {
  override def required = false
  override def initialVal = Some(value)
  override def widgetAttrs(widget: Widget) = {
    super.widgetAttrs(widget).append(Attribute("readonly", Text("readonly"), Null))
  }
}