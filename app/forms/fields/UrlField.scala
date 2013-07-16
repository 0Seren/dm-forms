package forms.fields

import java.net.{MalformedURLException, URL}

import forms.validators._
import forms.widgets.TextInput

trait BaseUrlField[T] extends Field[T] {
  override def widget = new TextInput(required, _inputType="url")
}

class UrlField(name: String) extends Field[URL](name) with BaseUrlField[URL] {
  override def asValue(strs: Seq[String]) = strs match {
    case Seq(str) => Right(new URL(str))
    case _ => Left(ValidationError("Expected a single URL, but got none or many."))
  }
} 

class UrlFieldOptional(name: String) extends Field[Option[URL]](name) with BaseUrlField[Option[URL]] {
  override def asValue(strs: Seq[String]) = strs match {
    case Seq() => Right(None)
    case Seq(str) => Right(Some(new URL(str)))
    case _ => Left(ValidationError("Expected zero or one URLs, but got many."))
  }
}