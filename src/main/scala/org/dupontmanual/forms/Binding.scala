package org.dupontmanual.forms

import org.dupontmanual.forms.validators.ValidationError
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import org.dupontmanual.forms.fields.Field
import play.api.mvc.MultipartFormData.FilePart
import org.dupontmanual.forms.fields.BaseFileField

/** The companion object for the abstract class `Binding`. The object's `apply`
  * methods are used to combine forms with data, which triggers form validation.
  *
  * Here's a typical use case:
  *
  * in a controller:
  * {{{
  * def handleGetRequest() = Action { implicit req =>
  * Ok(some.view(Binding(FormObject)))
  * }
  *
  * def handlePostRequest() = Action { implicit req =>
  * Binding(FormObject, req) match {
  *   case bindingWithErrors: InvalidBinding => Ok(some.view(bindingWithErrors)) // use same template
  *   case goodBinding: ValidBinding => {
  *     val datum = goodBinding.valueOf(FormObject.field)
  *     ...
  *   }
  * }
  * }
  * }}}
  *
  * In the view, you would take a `Binding` as a parameter and call `binding.render()`
  * somewhere to display the form.
  */
object Binding {
  /** use this version to create the form initially with default data */
  def apply(form: Form): InitialBinding = {
    new InitialBinding(form)
  }

  /** this version of the apply method grabs any data and files from
    * the POST request, packages them appropriately, and calls one
    * of the later forms
    */
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

  /** Creates a `Binding` of the given form with data. This version of the method is mostly
    * used for testing, since it's much easier to use the version that accepts the request
    * itself, which puts the data in correct format and then calls this method.
    * @param rawData data as would be received in a POST request, with the name of each field
    * and a `Seq[String]` represented any attached values
    * @param files a (possibly empty) `Seq[FilePart[_]]` that could be sent from a form with
    * attribute `enctype="multipart/form-data"`
    */
  def apply(form: Form, rawData: Map[String, Seq[String]], files: Seq[FilePart[_]]): Binding = {
    val valuesOrErrors: List[(String, Either[ValidationError, Any])] = form.fields.map(f => (f.name,
      f match {
        case t: BaseFileField[_] => t.cleanFiles(files)
        case _ => f.clean(f.widget.valueFromDatadict(rawData, f.name))
      }))
    val (values, errors) = valuesOrErrors.partition(_._2.isRight)
    val fieldErrors: Map[String, ValidationError] = Map(errors.map(nmEr => (nmEr._1, nmEr._2.left.get)): _*)
    val cleanedData: Map[String, Any] = Map(values.map(nmVal => (nmVal._1, nmVal._2.right.get)): _*)
    val validSoFar: ValidBinding = new ValidBinding(form, rawData, files, cleanedData)
    val formErrors: ValidationError = if (fieldErrors.isEmpty) form.validate(new ValidBinding(form, rawData, files, cleanedData)) else ValidationError(Nil)
    if (fieldErrors.isEmpty && formErrors.isEmpty) validSoFar else new InvalidBinding(form, rawData, files, fieldErrors, formErrors)
  }

  /** Creates a `Binding` of the given form with the given data and files. This is a convenience
    * method for testing which allows you to use a `Map[String, String]` for the raw data, rather
    * than the more general `Map[String, Seq[String]]`.
    */
  def apply(form: Form, rawData: Map[String, String], files: Seq[FilePart[_]])(implicit d: DummyImplicit): Binding = {
    apply(form, rawData.map {
      case (name, value) => (name, List(value))
    }, files)
  }
}

/** A `Binding` is a `Form` that has been attached to some data. There are three
  * subclasses of `Binding`: `InitialBinding`, `InvalidBinding`, and `ValidBinding`.
  *
  * Each `Binding` knows how to `render` itself as HTML, including displaying errors
  * for the form as a whole, or for individual fields.
  */
abstract class Binding(val form: Form, val rawData: Map[String, Seq[String]], val files: Seq[FilePart[_]]) {
  def formErrors: ValidationError = new ValidationError(Nil)
  def fieldErrors: Map[String, ValidationError] = Map()
  def fieldErrors(field: Field[_]): Option[ValidationError] = fieldErrors.get(field.name)
  def hasErrors: Boolean = !(formErrors.isEmpty && fieldErrors.isEmpty)
  def render(overrideSubmit: Option[FormCall] = None, legend: Option[String] = None): NodeSeq = form.render(this, overrideSubmit, legend)

  def asStringSeq(field: Field[_]): Seq[String] = {
    rawData.getOrElse(field.name, Nil)
  }
}

/** An `InitialBinding` is created the first time a `Form` is presented to the user.
  * It binds each field to its initial data.
  */
class InitialBinding(form: Form) extends Binding(form, Map(), Nil) {
  override def asStringSeq(field: Field[_]): Seq[String] = {
    field.initial
  }
}

/** An `InvalidBinding` indicates that validation errors occurred as the form
  * was bound to its data. Because of that, the cleaned data is not available, and
  * the form should be displayed for the user again so that the errors can be
  * corrected.
  */
class InvalidBinding(form: Form, rawData: Map[String, Seq[String]], files: Seq[FilePart[_]], _fieldErrors: Map[String, ValidationError], _formErrors: ValidationError)
  extends Binding(form, rawData, files) {
  override def formErrors = _formErrors
  override def fieldErrors = _fieldErrors
}

/** A `ValidBinding` indicates that form was combined with its data and no
  * errors occurred. The data entered into the form can now be retrieved using
  * the `valueOf` methods.
  */
class ValidBinding(form: Form, rawData: Map[String, Seq[String]], files: Seq[FilePart[_]], val cleanedData: Map[String, Any])
  extends Binding(form, rawData, files) {
  override def hasErrors: Boolean = false

  def valueOf[T](field: Field[T]): T = {
    cleanedData(field.name).asInstanceOf[T]
  }
}

