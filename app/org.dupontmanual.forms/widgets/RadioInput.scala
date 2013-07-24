package forms.widgets

import scala.xml._

/**
 * A widget that creates a list of radio buttons.
 */
class RadioInput(
    required: Boolean,
    val options: List[String],
    attrs: MetaData = Null) extends Widget(required, attrs) {

  /**
   * Renders the radio buttons using xml.
   */
  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    <fieldset name={ name }>{
      options.zipWithIndex.flatMap { vi => {
    	  if (value.contains(vi._2.toString)) <input type="radio" name={name} value={vi._2.toString} checked="true"/> ++ Text(vi._1) ++ <br/>
    	  else <input type="radio" name={name} value={vi._2.toString}/> ++ Text(vi._1) ++ <br/>
      	}
      }
    }</fieldset> % attrs % attrList
  }
}