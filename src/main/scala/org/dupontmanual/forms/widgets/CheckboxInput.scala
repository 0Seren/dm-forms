package org.dupontmanual.forms.widgets

import scala.xml._

/**
 * A widget that creates a list of checkboxes.
 */
class CheckboxInput(
    required: Boolean,
    val options: List[String],
    allowMultiple: Boolean = false,
    attrs: MetaData = Null) extends Widget(required, attrs) {

  /**
   * Renders the list of checkboxes using xml.
   */
  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    <fieldset name={ name }>{
      options.zipWithIndex.flatMap { case (valName, index) => {
        val attr = new UnprefixedAttribute("checked", 
            if (value.contains(index.toString)) Some(Text("checked")) else None, Null)
        <input type="checkbox" name={name} value={index.toString}/> % attr ++ Text(valName) ++ <br/>
      	}
      }
    }</fieldset> % attrs % attrList
  }
}
