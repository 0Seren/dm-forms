package forms.widgets

import scala.xml._

class RadioButton(
    required: Boolean,
    val options: List[String],
    attrs: MetaData = Null) extends Widget(required, attrs) {

  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    <fieldset name={ name }>
    	{
    	  var i = 0
    	  for(v <- value) {
    	    <input type="radio" name={name} value={i.toString}>{v}</input>
    	    i += 1
    	  }
    	}
    </fieldset> % attrs % attrList
  }
}