package forms.validators

import scala.xml.{NodeSeq, Text}

/**
 * A ValidationError is a list of NodeSeq that explains what is wrong
 * with a Field or Form. An empty ValidationError means nothing is wrong.
 */
class ValidationError(val messages: List[NodeSeq]) extends Seq[NodeSeq] {
  def apply(idx: Int) = messages(idx)
  def iterator = messages.iterator
  def length = messages.length
  
  def render: NodeSeq = messages match {
    case Nil => NodeSeq.Empty
    case _ => messages.flatMap(msg => 
    <span class="help-inline"><i class="icon-warning-sign"></i>&nbsp;{msg}</span>)
      
    /*{
      val uuid=java.util.UUID.randomUUID().toString()
      <a data-placement="left" title={"Validation Error" + "<button type=\"button\" id=\"close\" class=\"close\" onclick=\"$('."+ uuid +"').popover('hide');\">&times;</button>"} data-content={msg} data-html="true" class={"errorpopover "+uuid}>&nbsp;</a>
    })*/
      //messages.flatMap(msg => <div class="alert alert-error">{ msg }</div>)
  }
}

object ValidationError {
  def apply(message: String): ValidationError = ValidationError(List(Text(message)))
    
  def apply(messages: List[NodeSeq]): ValidationError = new ValidationError(messages)
}