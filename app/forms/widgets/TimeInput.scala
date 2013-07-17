package forms.widgets

import scala.xml._
import java.util.UUID
import forms.validators.ValidationError
import java.sql.Time

import scalatags._

/**
 * A widget that creates a time field using Bootstrap-Timepicker.
 */
class TimeInput(
  required: Boolean,
  attrs: MetaData = Null) extends Widget(required, attrs) { 

  /**
   * Renders the time field using xml.
   */
  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    val theValue = if (value.isEmpty) "" else value(0)
    <div class="input-append bootstrap-timepicker">
         {<input id="timepicker3" name={name} placeholder="hh:mm A/PM" type="text" class="timepicker" value={theValue} /> % attrs % reqAttr % attrList} ++ <span class="add-on"><i class="icon-time"></i></span>
    </div>
  }
  
  /**
   * Creates the scripts for the bootstrap-timepicker.
   */
  override def scripts: NodeSeq =
    <script type="text/javascript">
        $('.timepicker').timepicker({{
            minuteStep: 5,
		  	defaultTime: false
        }});
    </script>//<script>
	//	jQuery(function($) {{
	//  		$.mask.definitions['5']='[012345]';
	//  		$.mask.definitions['1']='[01]';
	//	  	$.mask.definitions['`']='[apAP]';
	//	  	$('.timepicker').mask('19:59 `M', {{ placeholder:'_' }});
	//	}});
  	//</script>
    
  /*  <script type="text/javascript"
     src="http://cdnjs.cloudflare.com/ajax/libs/jquery/1.8.3/jquery.min.js">
    </script> 
    <script type="text/javascript"
     src="http://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.2/js/bootstrap.min.js">
    </script>
    <script type="text/javascript"
     src="http://tarruda.github.com/bootstrap-datetimepicker/assets/js/bootstrap-datetimepicker.min.js">
    </script>
    <script type="text/javascript"
     src="http://tarruda.github.com/bootstrap-datetimepicker/assets/js/bootstrap-datetimepicker.pt-BR.js">
    </script>
    <script type="text/javascript">
      $(function(){{
		  $('.timepicker').datetimepicker({{
		  	pickDate: false,
		  	pickSeconds: false,
		  	pick12HourFormat: true
		  }});
	  }});
    </script>
    */
    
    /*Seq(script.ctype("text/javascript")(
		showPeriod: true,
    	showLeadingZero: true
      });
    });"""),
	script.ctype("text/javascript")(
    """jQuery(function($) {
	  	$.mask.definitions['5']='[012345]';
	  	$.mask.definitions['1']='[01]';
		$.mask.definitions['`']='[apAP]';
		$('.timepicker').mask('19:59 `M', { placeholder:'_' });
  	}});""")).toXML*/
}