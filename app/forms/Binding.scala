package forms

import validators.ValidationError
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import fields.Field
import play.api.mvc.MultipartFormData.FilePart
import forms.fields.BaseFileField

object Binding {
  def apply(form: Form): InitialBinding = {
    new InitialBinding(form)
  }
  
  def apply(form: Form, rawData: Map[String, Seq[String]], files: Seq[FilePart[_]]): Binding = {
    val valuesOrErrors: List[(String, Either[ValidationError, Any])] = form.fields.map(f => (f.name, 
      f match{ 
      	case t: BaseFileField[_] => t.cleanFiles(files)
      	case _ => f.clean(f.widget.valueFromDatadict(rawData, f.name))
      }
    ))
    val (values, errors) = valuesOrErrors.partition(_._2.isRight)
    val fieldErrors: Map[String, ValidationError] = Map(errors.map(nmEr => (nmEr._1, nmEr._2.left.get)): _*)
    val cleanedData: Map[String, Any] = Map(values.map(nmVal => (nmVal._1, nmVal._2.right.get)): _*)
    val validSoFar: ValidBinding = new ValidBinding(form, rawData, files, cleanedData)
    val formErrors: ValidationError = if (fieldErrors.isEmpty) form.validate(new ValidBinding(form, rawData, files, cleanedData)) else ValidationError(Nil)
    if (fieldErrors.isEmpty && formErrors.isEmpty) validSoFar else new InvalidBinding(form, rawData, files, fieldErrors, formErrors)
  }
  
  def apply(form: Form, rawData: Map[String, String], files: Seq[FilePart[_]])(implicit d: DummyImplicit): Binding = {
    apply(form, rawData.map {
      case (name, value) => (name, List(value))
    }, files)
  }
  
  def apply(form: Form, request: play.api.mvc.Request[_]): Binding = {
    val files: Seq[FilePart[_]] = request.body match {
    	case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.files
    	case body: play.api.mvc.MultipartFormData[_] => body.files
    	case _ => Seq()
  	}

    apply(form, 
      (request.body match {
        case body: play.api.mvc.AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
        case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
        //case body: play.api.mvc.AnyContent if body.asJson.isDefined => FormUtils.fromJson(js = body.asJson.get).mapValues(Seq(_))
        case body: Map[_, _] => body.asInstanceOf[Map[String, Seq[String]]]
        case body: play.api.mvc.MultipartFormData[_] => body.asFormUrlEncoded
        //case body: play.api.libs.json.JsValue => FormUtils.fromJson(js = body).mapValues(Seq(_))
        case _ => Map.empty[String, Seq[String]]
      }) ++ request.queryString,
      files)
  }
}

abstract class Binding(val form: Form, val rawData: Map[String, Seq[String]], val files: Seq[FilePart[_]]) {
  def formErrors: ValidationError = new ValidationError(Nil)
  def fieldErrors: Map[String, ValidationError] = Map()
  def fieldErrors(field: Field[_]): Option[ValidationError] = fieldErrors.get(field.name)
  def hasErrors: Boolean = !(formErrors.isEmpty && fieldErrors.isEmpty)
  def render(overrideSubmit: Option[FormCall]=None, legend: Option[String]=None): NodeSeq = form.render(this, overrideSubmit, legend)
  
  def asStringSeq(field: Field[_]): Seq[String] = {
    rawData.getOrElse(field.name, Nil)
  }
}

class InitialBinding(form: Form) extends Binding(form, Map(), Nil) {
  override def asStringSeq(field: Field[_]): Seq[String] = {
    field.initial
  }
}

class InvalidBinding(form: Form, rawData: Map[String, Seq[String]], files: Seq[FilePart[_]], _fieldErrors: Map[String, ValidationError], _formErrors: ValidationError)
	extends Binding(form, rawData, files) {
  override def formErrors = _formErrors
  override def fieldErrors = _fieldErrors
}

class ValidBinding(form: Form, rawData: Map[String, Seq[String]], files: Seq[FilePart[_]], val cleanedData: Map[String, Any])
	extends Binding(form, rawData, files) {
  override def hasErrors: Boolean = false
  
  def valueOf[T](field: Field[T]): T = {
    cleanedData(field.name).asInstanceOf[T]
  }
}

