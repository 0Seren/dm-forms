package forms.widgets

import scala.xml._
import java.util.UUID
import forms.validators.ValidationError

import scalatags._

class DateInput(
  required: Boolean,
  attrs: MetaData = Null) extends Widget(required, attrs) {
  
  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    val theValue = if (value.isEmpty) "" else value(0)
    <div class="input-append date">
	  {<input type="text" class="datepicker" placeholder="mm/dd/yyyy" name={name} value={theValue} /> % attrs % reqAttr % attrList} ++ <span class="add-on"><i class="icon-th"></i></span>
    </div>
  }
  
  override def scripts: NodeSeq = 
    <script>
    $('.input-append.date').datepicker({{
    	autoclose: true,
		forceParse: false
    }});
	</script><script>
	jQuery(function($){{
		$('.datepicker').mask('99/99/99?99',{{ placeholder:'_' }});
  	}});
	</script>
    
    
    /*Seq(script.ctype("text/javascript")(
		  changeMonth: true,
  		  changeYear: true,
  		  altField: '.${theUuid}',
  		  altFormat: 'DD, d MM, yy',
  		  showOtherMonths: true,
  		  selectOtherMonths: true,
  		  showOn: 'both',
  		  buttonImageOnly: true,
  		  buttonImage: '/assets/images/calendar.jpeg',
  		  shortYearCutoff: 99,
  		  buttonText: 'Chooser'
      });
    });"""),
    script.ctype("text/javascript")(
	"""jQuery(function($){
		$('.datepicker').mask('99/99/99?99',{ placeholder:'_' });
  	}});""")).toXML*/
}