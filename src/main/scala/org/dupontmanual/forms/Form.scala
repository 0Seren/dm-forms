package forms

import scala.xml._
import forms.fields._
import forms.widgets.Widget
import forms.validators.ValidationError
import play.api.mvc.Request
import play.api.templates.Html
import scala.reflect.runtime.universe._

import fields._
import validators.ValidationError

/**
 * Used as the template for all forms to be based off of.
 */
abstract class Form {
  // TODO: check that all names are unique
  
  /**
   * The list of fields in the form in order that they appear.
   */
  def fields: List[Field[_]]
  
  /**
   * Lets the Binding know if there are any form errors that occur.
   * Defualt set to Nil.
   */
  def validate(data: ValidBinding): ValidationError = new ValidationError(Nil)
  
  /**
   * Determines where the page will redirect to when the cancel button is clicked.
   * None makes it set to go back to the previous page.
   */
  def cancelTo: Option[Call] = None
  
  /**
   * Determines what method the submit button calls.
   * Default set to POST.
   */
  def submitMethod: FormMethod = Method.POST
  
  /**
   * Determines what url the submit button redirects to. 
   * By default it goes to the same page the form was displayed on. 
   * Can redirect from controllers.
   */
  def submitUrl: Option[String] = None
  
  /**
   * Determines the prefix that gets added onto all the fields' names.
   * Default is none.
   */
  def prefix: Option[String] = None
  
  /**
   * Provides the format for creating the id for each field.
   * Default is "id_%s", where %s is the name of the field.
   * To change what %s is, edit Field.id(form: Form): Option[String].
   */
  def autoId: Option[String] = Some("id_%s")
  
  /**
   * Determines what text appears on the submit button.
   * Default is "Submit".
   */
  def submitText = "Submit"
    
  /**
   * Determines if there will or will not be a cancel button included on the form.
   * Default set to true.
   */
  def includeCancel = true
  
  /**
   * Determines what text appears on the cancel button.
   * Default set to "Cancel".
   */
  def cancelText = "Cancel"
    
  /**
   * Sets the method type and action for the submit button.
   */
  protected def methodPlusAction(overrideSubmit: Option[FormCall]): 
      (FormMethod, Option[String]) = overrideSubmit match {
    case Some(FormCall(meth, url)) => (meth, url)
    case None => (submitMethod, None)
  }
  
  /**
   * Renders the entire form for display on the html page. Includes the legend, form errors, fields, action buttons, and field scripts.
   */ 
  def render(bound: Binding, overrideSubmit: Option[FormCall]=None, legend: Option[String]=None): NodeSeq = {
    val (method: FormMethod, action: Option[String]) = methodPlusAction(overrideSubmit)
    <form id="form" method={ method.forForm } action={ action.map(Text(_)) } enctype={if(fields.map(x => x match{ case f:BaseFileField[_] => true; case _ => false }).contains(true)) "multipart/form-data" else "application/x-www-form-urlencoded"} class="form-horizontal well offset1 span7" >
    	<fieldset>
        { legend.map(txt => <legend>{ txt }</legend>).getOrElse(NodeSeq.Empty) }
        { bound.formErrors.render }
        { fields.flatMap(_.render(bound)) }
        { actions }
      </fieldset>
      { this.scripts ++ <script type="text/javascript">
      		$(function () {{ 
      			$('.formtooltip').tooltip();
      		}});
      </script>}
    </form>
  }
  
  /**
   * Combines all widget scripts into one NodeSeq, and deletes any duplicates.
   */
  def scripts: NodeSeq = {
    fields.flatMap(_.widget.scripts).distinct
  }
  
  /**
   * Creates the action buttons for the form: Submit button and Cancel button. 
   */
  def actions: NodeSeq = {
    <div class="form-actions">
      <button type="submit" class="btn btn-primary">{ submitText }</button>
      {
        if (!includeCancel) NodeSeq.Empty
        else {
          val onClick = cancelTo.map(call => s"window.location.href=${call.url}").getOrElse("window.history.back()")
          <button type="button" class="btn" onclick={ onClick }>{ cancelText }</button>
        }
      }
    </div>
  }
  
  /**
   * Sees if any field requires MultiPartFormData.
   */  
  def requiresMultipartData: Boolean = fields.exists(_.requiresMultipartData)
  
  /**
   * Adds the form prefix to the fields' names.
   */
  def addPrefix(fieldName: String): String = {
    prefix.map(p => "%s-%s".format(p, fieldName)).getOrElse(fieldName)
  }
  
  /**
   * Adds the inital prefix to the fields' initial names.
   */
  def addInitialPrefix(fieldName: String): String = {
    "initial-%s".format(addPrefix(fieldName))
  }
}



