package forms.widgets

import scala.xml._
import java.util.UUID
import forms.validators.ValidationError
import java.sql.Timestamp

/**
 * A widget that groups together a DateInput and a TimeInput widget.
 */
class DateTimeInput(
    val fieldName: String,
    required: Boolean,
    attrs: MetaData = Null) extends Widget(required, attrs) {
  
  /**
   * Creates the DateInput widget
   */
  val dateWidget = new DateInput(required, attrs)
  
  /**
   * Creates the TimeInput widget.
   */
  val timeWidget = new TimeInput(required, attrs)
  
  /**
   * Renders a DateInput widget and a TimeInput widget one below the other.
   */
  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    dateWidget.render(name, value, attrList) ++ <br/> ++ timeWidget.render(name, value, attrList)
  }
  
  /**
   * Combines the scripts from both the DateInput and the TimeInput widgets.
   */
  override def scripts: NodeSeq = {
    dateWidget.scripts ++ timeWidget.scripts
  }
}