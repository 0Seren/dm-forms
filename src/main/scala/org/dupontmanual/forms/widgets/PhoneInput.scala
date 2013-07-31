package org.dupontmanual.forms.widgets

import scala.xml._
import org.dupontmanual.forms.validators.ValidationError

/**
 * A widget that creates a new phone field.
 */
class PhoneInput(
  required: Boolean,
  attrs: MetaData = Null,
  useMaskedInputs: Boolean = false,
  placeHolder: Char = '_',
  phoneFormat: String = "###-###-####") extends Widget(required, attrs) {
  
  /**
   * Renders the phone field on the page using xml.
   */
  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    <input type="text" name={ name } value={if(value.isEmpty) "" else value(0) } placeholder={phoneFormat} class="phone"></input> % attrs % reqAttr % attrList
  }
  
  /**
   * Creates the masked input scripts for phone field.
   */
  override def scripts: NodeSeq ={
    if(useMaskedInputs){
     val format = phoneFormat.map((c: Char) => if(c == '#') '9' else c)
     <script type="text/javascript">
     jQuery(function($){{
    	$('.phone').mask('{format}', {{placeholder:'{placeHolder}'}});
     }}
     </script>
    } else NodeSeq.Empty
  }

}
