package forms.validators

import scala.xml.{NodeSeq, Text}

/**
 * A ValidationError is a list of NodeSeq that explains what is wrong
 * with a Field or Form. An empty ValidationError means nothing is wrong.
 */
class ValidationError(val messages: List[NodeSeq]) extends Seq[NodeSeq] {
  
  /**
   * Returns the message at that index.
   */
  def apply(idx: Int) = messages(idx)
  
  /**
   * Creates an iterator for the messages.
   */
  def iterator = messages.iterator
  
  /**
   * Determines how many messages there are.
   */
  def length = messages.length
  
  /**
   * Renders each individual message using Bootstrap.
   */
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

/**
 * Helper for class ValidationError.
 */
object ValidationError {
  
  /**
   * Creates a new ValidationError from a String.
   */
  def apply(message: String): ValidationError = ValidationError(List(Text(message)))
  
  /**
   * Creates a new ValidationError from a List[NodeSeq]
   */
  def apply(messages: List[NodeSeq]): ValidationError = new ValidationError(messages)
}