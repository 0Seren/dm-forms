package forms.fields

import scala.xml._
import forms.widgets._
import forms.validators._
import forms.Form
import forms.Binding
import play.api.templates.Html
import scala.reflect.runtime.universe._

/**
 * A template for fields that are to be included in a form.
 */
abstract class Field[T](val name: String)(implicit tag: TypeTag[T]) {
  
  /**
   * The list of validators that are used to make sure the user's input is correct.
   */
  def validators: List[Validator[T]] = Nil
  
  /**
   * Sets a required attribute for the field to see if the field
   * is required or not. Is default set to false if the return type
   * is an Option and true if not.
   */
  def required: Boolean = !(typeOf[T] <:< typeOf[Option[_]])
  
  /**
   * Sets the widget that is used for the Field.
   */
  def widget: Widget = new TextInput(required)
  
  /**
   * Gets set to true when the field requires Multipart Form Data.
   * Multipart Form Data is required for FileFields.
   */
  def requiresMultipartData: Boolean = widget.needsMultipartForm
  
  /**
   * Creates the Seq[String] from initialVal that is used to set the initial value for the field.
   */
  def initial: Seq[String] = asStringSeq(initialVal)
  
  /**
   * Determines what the initial value of the field will be.
   */
  def initialVal: Option[T] = None
  
  /**
   * Creates a label for the field from the field name.
   */
  def label: Option[NodeSeq] = Some(Text(Field.camel2TitleCase(name)))
  
  /**
   * Sets the id for the field while adhering to the format set in the form.
   */
  def id(form: Form): Option[String] = form.autoId.map(_.format(name))
  
  /**
   * An Option[NodeSeq] that is used to help explain to the user something about the specific field.
   * Uses bootstrap to display.
   */
  def helpText: Option[NodeSeq] = None
  
  /**
   * Determines if only spaces in a field should be treated the same way as a non-filled in field.
   */
  def spacesSameAsBlank = true
  
  /**
   * Displays the field label with it's widget, help text and any errors
   * that occur with the field.
   */
  def render(bound: Binding): NodeSeq = {
    val errors = bound.fieldErrors.get(name).map(_.render).getOrElse(NodeSeq.Empty)
    <div class={"control-group " + {if(errors.isEmpty) "" else "error"}}>
      { labelElem(bound.form) }
      <div class="controls">
      	{ helpText.map((text: NodeSeq) => <i data-placement="top" title={text} data-html="true" class="formtooltip icon-question-sign"></i>).getOrElse(NodeSeq.Empty) }
        { asWidget(bound) }
        { errors }
        <br />
      </div>  
    </div>
  }
  
  /**
   * Returns the label for the field as a NodeSeq
   */
  def labelElem(form: Form): NodeSeq = (label, id(form)) match {
    case (Some(label), Some(id)) => <label class={"control-label "+label} for={ id }>{ label }</label>
    case (Some(label), None) => <label class={"control-label "+label}>{ label }</label>
    case _ => NodeSeq.Empty
  }
  
  /**
   * Determines the default height of a field. Not used for fileFields, radioFields,
   * or checkboxFields. Used to make sure text isn't cut off at the top and bottom
   * and that fields line up with any icons/addons
   */
  val defaultHeight = "30px"
    
  /**
   * Renders the widget, and sends it any xml attributes that the widget should include, such as height and id.
   */
  def asWidget(bound: Binding, widget: Widget = widget, attrs: MetaData = Null, onlyInitial: Boolean = false): NodeSeq = {
    val idAttr = if (autoId(bound.form).isDefined && attrs.get("id").isEmpty && widget.attrs.get("id").isEmpty) {
      new UnprefixedAttribute("id", Text(if (!onlyInitial) autoId(bound.form).get else htmlInitialId(bound.form)), Null)
    } else {
      Null
    }
    widget.render(if (!onlyInitial) htmlName(bound.form) else htmlInitialName(bound.form), bound.asStringSeq(this), attrs.append(idAttr).append(
        if((this.isInstanceOf[BaseRadioField[_,_]] || this.isInstanceOf[BaseCheckboxField[_,_]] || this.isInstanceOf[BaseFileField[_]])) Null
        else new UnprefixedAttribute("style", "height:"+defaultHeight, Null)))
  }

  private[this] lazy val _errorMessages: Map[String, String] = {
    Map("required" -> "This field is required.",
        "invalid" -> "Enter a valid value.")
  }
  
  /**
   * Sets some easy to use, global error messages, so messages al appear the same way.
   */
  def errorMessages = _errorMessages
  
  /**
   * Determines how the user's input will be converted to a string before being
   * processed in asValue.
   */
  def asStringSeq(value: Option[T]): Seq[String] = value match {
    case Some(Some(t)) => if(required) List(Some(t).toString) else List(t.toString)
  	case Some(t) => if(!t.equals(None)) List(t.toString) else Nil
    case None => Nil
  }
  
  /**
   * Returns an object that is represenative of the user's selection. Optional fields
   * will return an Option[T]
   */
  def asValue(s: Seq[String]): Either[ValidationError, T]
  
  /**
   * Cleans the rawData by making sure there are no validationErrors.
   */
  def clean(rawData: String): Either[ValidationError, T] = {
    clean(List(rawData))
  }
  
  /**
   * Cleans the rawData by making sure there are no validationErrors.
   */
  def clean(rawData: Seq[String]): Either[ValidationError, T] = {
    checkRequired(rawData).fold(
        Left(_), 
        asValue(_).fold(
        	Left(_), validate(_)))
  }
  
  /**
   * Makes sure that a value has been input for required fields. 
   */
  def checkRequired(rawData: Seq[String]): Either[ValidationError, Seq[String]] = {
    rawData match {
      case Seq() => if (this.required) Left(ValidationError(errorMessages("required")))
      	  else Right(rawData)
      case Seq(strs@_*) => {
        if (strs.exists(s => (if (spacesSameAsBlank) s.trim else s) != "")) {
          Right(rawData)
        } else if (required) {
          Left(ValidationError(errorMessages("required")))
        } else {
          Right(Nil)
        }
      } 
    }
  }
  
  /**
   * Returns the name of the field to be set as an html attribute after adding on the form's prefix.
   */
  def htmlName(form: Form) = form.addPrefix(name)
  
  /**
   * Returns the name of the field to be set as an html attribute after adding on the form's initial prefix.
   */
  def htmlInitialName(form: Form) = form.addInitialPrefix(name)
  
  /**
   * Creates the id attribute for the field using the form's initial prefix.
   */
  def htmlInitialId(form: Form) = form.addInitialPrefix(autoId(form).getOrElse(""))
  
  /**
   * Creates the id for the field while adhering to the format in Form.
   */
  def autoId(form: Form): Option[String] = {
    val htmlName = form.addPrefix(name)
    form.autoId.map(id => {
      if (id.contains("%s")) id.format(htmlName)
      else htmlName
    })
  }
  
  /**
   * Determines if there are ValidationErrors from the list of validators
   */
  def validate(value: T): Either[ValidationError, T] = {
    val errors = ValidationError(this.validators.flatMap(_.apply(value)))
    if (errors.isEmpty) Right(value) else Left(errors)
  } 
  
  /**
   * Returns the bound data.
   */
  def boundData(data: Seq[String], initial: Seq[String]): Seq[String] = data 
  
  /**
   * Determines a set of attributes that are to be set in the widget.
   */
  def widgetAttrs(widget: Widget): MetaData = Null
}

object Field {
  
  /**
   * Turns a string formatted in camel case to title case.
   */
  def camel2TitleCase(camel: String): String = {
    camel match {
      case s: String if (s.length > 0) => {
        val buf: StringBuilder = new StringBuilder(s.substring(0, 1).toUpperCase)
        (1 until s.length) foreach ((index: Int) => {
          if ((index < s.length - 2) && 
              Character.isUpperCase(s.charAt(index - 1)) &&
              Character.isUpperCase(s.charAt(index)) &&
              Character.isLowerCase(s.charAt(index + 1))) {
            buf ++= (" " + s.substring(index, index + 1))
          } else if (Character.isUpperCase(s.charAt(index)) && 
              !Character.isUpperCase(s.charAt(index - 1))) {
            buf ++= (" " + s.substring(index, index + 1))
          } else if (!Character.isLetter(s.charAt(index)) &&
              Character.isLetter(s.charAt(index - 1))) {
            buf ++= (" " + s.substring(index, index + 1))
          } else {
            buf += s.charAt(index)
          }
        })
        buf.toString
      }
      case _ => camel
    }
  }
}
