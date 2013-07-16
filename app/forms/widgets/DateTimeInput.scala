package forms.widgets

import scala.xml._
import java.util.UUID
import forms.validators.ValidationError
import java.sql.Timestamp

class DateTimeInput(
    val fieldName: String,
    required: Boolean,
    attrs: MetaData = Null) extends Widget(required, attrs) {
  
  val dateWidget = new DateInput(required, attrs)
  val timeWidget = new TimeInput(required, attrs)
  
  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    dateWidget.render(name, value, attrList) ++ <br/> ++ timeWidget.render(name, value, attrList)
  }
  
  override def scripts: NodeSeq = {
    dateWidget.scripts ++ timeWidget.scripts
  }
}