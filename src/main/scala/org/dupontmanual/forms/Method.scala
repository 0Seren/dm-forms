package forms

import scala.language.implicitConversions
import scala.xml.Elem
import play.api.mvc.{ Call => PlayCall }

/** A class representing the generally recognized HTTP methods.
 *  This is used to provide compile-time checking that is not
 *  achievable if you use a `String` for the method type.
 */
sealed abstract class Method(value: String) {
  /** returns the method as all upper case, `e.g.`, GET, POST, HEAD */
  def forRequest: String = value
}

/** A subclass of `Method` limited to the HTTP methods valid for
 *  a form, i.e. GET and POST.
 */
sealed abstract class FormMethod(value: String) extends Method(value) {
  /** an all-lower-case form of the method (ex: `get` or `post`), suitable 
   *  for use in an HTML5 form element 
   */
  def forForm: String = value.toLowerCase
}

object FormMethod {
  def apply(method: String): FormMethod = method match {
    case "GET" => Method.GET
    case "POST" => Method.POST
  }
}

/** Provides case objects for the standard (and some non-standard?)
 *  HTTP methods.
 */
object Method {
  case object GET extends FormMethod("GET")
  case object POST extends FormMethod("POST")
  case object HEAD extends Method("HEAD")
  case object PUT extends Method("PUT")
  case object DELETE extends Method("DELETE")
  case object TRACE extends Method("TRACE")
  case object OPTIONS extends Method("OPTIONS")
  case object CONNECT extends Method("CONNECT")
  case object PATCH extends Method("PATCH")
  
  /** implicitly converts a Method to an all-upper-case `String`,
   *  as expected by the Play framework, for example
   */
  implicit def method2string(method: Method): String = method.forRequest
  
  def apply(method: String): Method = method match {
    case "GET" => GET
    case "POST" => POST
    case "HEAD" => HEAD
    case "PUT" => PUT
    case "DELETE" => DELETE
    case "TRACE" => TRACE
    case "OPTIONS" => OPTIONS
    case "CONNECT" => CONNECT
    case "PATCH" => PATCH
  }
}

/** A representation of an HTTP call, with the method and URL */
sealed class Call(val method: Method, val url: String) {
  /** converts the call to a simple XML element (for serialization, perhaps) */
  def toXml: Elem = <call><method>{ method.forRequest }</method><url>{ url }</url></call>
  
  /** returns the URL as a `String` */
  override def toString: String = url
}
/** A representation that can be used to populate the `action` and `method` attributes
 *  of an HTML `form` element. The method is limited to `GET` and `POST`, and the URL
 *  is optional, with a missing URL indicating that the form should be submitted back
 *  to the same URL.
 */
case class FormCall(formMethod: FormMethod, formUrl: Option[String]) extends Call(formMethod, formUrl.getOrElse(""))

/** Companion object for the `Call` class. Convenience methods for creating
 *  and converting `Call` instances.
 */
object Call {
  /** creates a `Call` with the given method and url */
  def apply(method: Method, url: String): Call = new Call(method, url)
  
  /** creates a `Call` from an XML element as created by the `Call.toXml`
   *  method. (May throw an exception if the XML is not formed as expected.)
   */
  def fromXml(call: Elem): Call = {
    val method: Method = Method((call \ "method")(0).text)
    val url: String = (call \ "url")(0).text
    Call(method, url)
  } 
  
  /** implicit conversions from these `Call` instances to `play.api.mvc.Call` or `String`
   */
  implicit def call2playCall(call: Call): PlayCall = PlayCall(call.method.forRequest, call.url)
  implicit def playCall2call(playCall: PlayCall): Call = Call(Method(playCall.method), playCall.url)
  implicit def call2string(call: Call): String = call.toString
}

/** Companion class to `FormCall`, with a convenience method for turning a 
 *  `play.api.mvc.Call` instance into an instance of `FormCall`.
 */
object FormCall {
  /** attempts to convert a `play.api.mvc.Call` into a `FormCall`. Could throw an
   *  exception if this is not a reasonable thing to do.
   */
  def apply(playCall: PlayCall): FormCall = FormCall(FormMethod(playCall.method), toOption(playCall.url))
  
  private def toOption(url: String): Option[String] = if (url == null || url == "") None else Some(url)
}

