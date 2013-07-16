package forms.fields

import scala.xml._
import forms.widgets._
import forms.validators._
import forms.Form
import forms.Binding
import play.api.templates.Html
import scala.reflect.runtime.universe._

abstract class Field[T](val name: String)(implicit tag: TypeTag[T]) {
  
  def validators: List[Validator[T]] = Nil

  def required: Boolean = !(typeOf[T] <:< typeOf[Option[_]])
  def widget: Widget = new TextInput(required)
  
  def requiresMultipartData: Boolean = widget.needsMultipartForm
  
  def initial: Seq[String] = asStringSeq(initialVal)  
  def initialVal: Option[T] = None
  
  def label: Option[NodeSeq] = Some(Text(Field.camel2TitleCase(name)))
  def id(form: Form): Option[String] = form.autoId.map(_.format(name))
  def helpText: Option[NodeSeq] = None
  def spacesSameAsBlank = true
  
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
    
  def labelElem(form: Form, errors: NodeSeq = NodeSeq.Empty): NodeSeq = (label, id(form)) match {
    case (Some(label), Some(id)) => <label class={"control-label "+label} for={ id }>{ errors ++ label }</label>
    case (Some(label), None) => <label class={"control-label "+label}>{ errors ++ label }</label>
    case _ => errors
  }
  
  val defaultHeight = "30px"

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
  def errorMessages = _errorMessages
  
  def asStringSeq(value: Option[T]): Seq[String] = value match {
    case Some(Some(t)) => if(required) List(Some(t).toString) else List(t.toString)
  	case Some(t) => if(!t.equals(None)) List(t.toString) else Nil
    case None => Nil
  }
  
  def asValue(s: Seq[String]): Either[ValidationError, T]
  
  def clean(rawData: String): Either[ValidationError, T] = {
    clean(List(rawData))
  }
  
  def clean(rawData: Seq[String]): Either[ValidationError, T] = {
    checkRequired(rawData).fold(
        Left(_), 
        asValue(_).fold(
        	Left(_), validate(_)))
  }
  
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
    
  def htmlName(form: Form) = form.addPrefix(name)
  def htmlInitialName(form: Form) = form.addInitialPrefix(name)
  def htmlInitialId(form: Form) = form.addInitialPrefix(autoId(form).getOrElse(""))
  
  def autoId(form: Form): Option[String] = {
    val htmlName = form.addPrefix(name)
    form.autoId.map(id => {
      if (id.contains("%s")) id.format(htmlName)
      else htmlName
    })
  }
  
  def validate(value: T): Either[ValidationError, T] = {
    val errors = ValidationError(this.validators.flatMap(_.apply(value)))
    if (errors.isEmpty) Right(value) else Left(errors)
  } 
  
  def boundData(data: Seq[String], initial: Seq[String]): Seq[String] = data 
  
  def widgetAttrs(widget: Widget): MetaData = Null
}

object Field {
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
