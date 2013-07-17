package forms

import scala.language.implicitConversions
import scala.xml.Elem
import play.api.mvc.{ Call => PlayCall }
import scalatags._


sealed abstract class Method(value: String) {
  def forRequest: String = value
}

sealed abstract class FormMethod(value: String) extends Method(value) {
  def forForm: String = value.toLowerCase
}

object FormMethod {
  def apply(method: String): FormMethod = method match {
    case "GET" => Method.GET
    case "POST" => Method.POST
  }
}

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

sealed class Call(val method: Method, val url: String) {
  def toXml: Elem = <call><method>{ method.forRequest }</method><url>{ url }</url></call>
  
  override def toString: String = url
}
case class FormCall(formMethod: FormMethod, formUrl: Option[String]) extends Call(formMethod, formUrl.getOrElse(""))

object Call {
  def apply(method: Method, url: String): Call = new Call(method, url)
  
  def fromXml(call: Elem): Call = {
    val method: Method = Method((call \ "method")(0).text)
    val url: String = (call \ "url")(0).text
    Call(method, url)
  } 
  
  implicit def call2playCall(call: Call): PlayCall = PlayCall(call.method.forRequest, call.url)
  implicit def playCall2call(playCall: PlayCall): Call = Call(Method(playCall.method), playCall.url)
  implicit def call2sTag(call: Call): STag = StringSTag(call.toString)
  implicit def call2string(call: Call): String = call.toString
}

object FormCall {
  def apply(playCall: PlayCall): FormCall = FormCall(FormMethod(playCall.method), toOption(playCall.url))
  
  def toOption(url: String): Option[String] = if (url == null || url == "") None else Some(url)
}

