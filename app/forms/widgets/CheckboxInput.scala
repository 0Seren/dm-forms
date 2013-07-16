package forms.widgets

import scala.xml._

class CheckboxInput(
    required: Boolean,
    val options: List[String],
    attrs: MetaData = Null) extends Widget(required, attrs) {

  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    <fieldset name={ name }>{
      options.zipWithIndex.flatMap { vi => {
    	  if (value.contains(vi._2.toString)) <input type="checkbox" name={name} value={vi._2.toString} checked="true"/> ++ Text(vi._1) ++ {if(vi._2 == 0) <br/> else <br/>}
    	  else <input type="checkbox" name={name} value={vi._2.toString}/> ++ Text(vi._1) ++ {if(vi._2 == 0) <br/> else <br/>}
      	}
      }
    }</fieldset> % attrs % attrList
  }
}