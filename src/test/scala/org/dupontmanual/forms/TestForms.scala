package org.dupontmanual.forms

import scala.xml.Utility.trim
import org.scalatest.FunSuite
import org.dupontmanual.forms.fields.{ NumericField, TextField }
import org.dupontmanual.forms.validators.ValidationError

class PersonForm extends Form {
  val firstName = new TextField("firstName")
  val lastName = new TextField("lastName")
  val age = new NumericField[Int]("age")
  
  def fields = List(firstName, lastName, age)
}
  
class TestForms extends FunSuite {
  test("bound form") {
    val f = new PersonForm()
    val b = Binding(f, Map("firstName" -> "John", "lastName" -> "Lennon", "age" -> "72"))
    assert(b.fieldErrors.isEmpty, "%s should be empty".format(b.fieldErrors))
    assert(b.formErrors.isEmpty, "%s should be empty".format(b.formErrors))
    assert(b.isInstanceOf[ValidBinding])
    val vb: ValidBinding = b.asInstanceOf[ValidBinding]
    assert(vb.valueOf(f.firstName) === "John")
    assert(vb.valueOf(f.lastName) === "Lennon")
    assert(vb.valueOf(f.age) === 72)
    assert(f.firstName.asWidget(b) === 
      <input type="text" name="firstName" value="John" id="id_firstName" required="required" style="height:30px" />)
    assert(f.lastName.asWidget(b) === 
      <input type="text" name="lastName" value="Lennon" id="id_lastName" required="required" style="height:30px" />)
    assert(f.age.asWidget(b) === 
      <input type="number" name="age" value="72" id="id_age" required="required" style="height:30px" />)
    assert(trim(<div>{ b.render() }</div>) === trim(<div>{ PersonForm.bound }</div>))
  }
  
  test("bind with empty Map") {
    val f = new PersonForm()
    val b = Binding(f, Map.empty[String, String])
    assert(b.isInstanceOf[InvalidBinding])
    val ib = b.asInstanceOf[InvalidBinding]
    assert(b.fieldErrors(f.firstName) === Some(ValidationError("This field is required.")))
    assert(b.fieldErrors(f.lastName) === Some(ValidationError("This field is required.")))
    assert(b.fieldErrors(f.age) === Some(ValidationError("This field is required.")))
    // if there are errors, boundFields is an empty Map
    assert(trim(<div>{ b.render() }</div>) === trim(<div>{ PersonForm.withRequiredErrors }</div>))
  }
  
  test("cleaned data only includes defined fields") {
    val f = new PersonForm()
    val b = Binding(f, Map("firstName" -> "John", "lastName" -> "Lennon", "age" -> "72",
            			   "extra1" -> "hello", "extra2" -> "hello"))
    assert(b.isInstanceOf[ValidBinding])
    val vb: ValidBinding = b.asInstanceOf[ValidBinding]
    assert(vb.valueOf(f.firstName) === "John")
    assert(vb.valueOf(f.lastName) === "Lennon")
    assert(vb.valueOf(f.age) === 72)
  } 
}

object PersonForm {
  def bound = 
<form id="form" method="post" enctype="application/x-www-form-urlencoded" class="form-horizontal well offset1 span7">
  <fieldset>
  <div class="control-group ">
    <label class="control-label First Name" for="id_firstName">First Name</label>
    <div class="controls">  	
      <input value="John" id="id_firstName" style="height:30px" required="required" type="text" name="firstName"/><br/>
    </div>  
  </div>
  <div class="control-group ">
    <label class="control-label Last Name" for="id_lastName">Last Name</label>
    <div class="controls">  	
      <input value="Lennon" id="id_lastName" style="height:30px" required="required" type="text" name="lastName"/><br/>
    </div>  
  </div>
  <div class="control-group ">
    <label class="control-label Age" for="id_age">Age</label>
    <div class="controls">  	
      <input value="72" id="id_age" style="height:30px" required="required" type="number" name="age"/><br/>
    </div>  
  </div>
  <div class="form-actions">
    <button type="submit" class="btn btn-primary">Submit</button>
    <button type="button" class="btn" onclick="window.history.back()">Cancel</button>
  </div>
</fieldset>
<script type="text/javascript">$(function () {{ 
    $('.formtooltip').tooltip();
  }});
</script>
</form>
  
  val withRequiredErrors = 
<form id="form" method="post" enctype="application/x-www-form-urlencoded" class="form-horizontal well offset1 span7">
<fieldset>
  <div class="control-group error">
    <label class="control-label First Name" for="id_firstName">First Name</label>
    <div class="controls">
      <input id="id_firstName" style="height:30px" required="required" type="text" name="firstName"/>
      <span class="help-inline"><i class="icon-warning-sign"></i>&nbsp;This field is required.</span><br/>
    </div>  
  </div>
  <div class="control-group error">
    <label class="control-label Last Name" for="id_lastName">Last Name</label>
    <div class="controls">
      <input id="id_lastName" style="height:30px" required="required" type="text" name="lastName"/>
      <span class="help-inline"><i class="icon-warning-sign"></i>&nbsp;This field is required.</span><br/>
    </div>  
  </div>
  <div class="control-group error">
    <label class="control-label Age" for="id_age">Age</label>
    <div class="controls">
      <input id="id_age" style="height:30px" required="required" type="number" name="age"/>
      <span class="help-inline"><i class="icon-warning-sign"></i>&nbsp;This field is required.</span><br/>
    </div>  
  </div>
  <div class="form-actions">
    <button type="submit" class="btn btn-primary">Submit</button>
    <button type="button" class="btn" onclick="window.history.back()">Cancel</button>
  </div>
</fieldset>
<script type="text/javascript">$(function () {{ 
    $('.formtooltip').tooltip();
  }});
</script>
</form>
}