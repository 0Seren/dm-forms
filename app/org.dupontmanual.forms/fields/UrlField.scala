package forms.fields

import java.net.{MalformedURLException, URL}

import forms.validators._
import forms.widgets.TextInput

/*
 * creates an input type of url and requires a correctly formatted url. Returns a java.net.URL
 */

/**
 * Creates methods used in UrlField and UrlFieldOptional.
 */
trait BaseUrlField[T] extends Field[T] {
  
  /**
   * Sets the widget for UrlField and UrlFieldOptional.
   */
  override def widget = new TextInput(required, _inputType="url")
}

/**
 * Creates a new required UrlField.
 */
class UrlField(name: String) extends Field[URL](name) with BaseUrlField[URL] {
  
  /**
   * Returns a java.net.URL if the input is in a valid format, or a
   * ValidationError if it's in an incorrect format or nothing was submitted.
   */
  override def asValue(strs: Seq[String]) = strs match {
    case Seq(str) => Right(new URL(str))
    case _ => Left(ValidationError("Expected a single URL, but got none or many."))
  }
} 

/**
 * Creates a new optional UrlField.
 */
class UrlFieldOptional(name: String) extends Field[Option[URL]](name) with BaseUrlField[Option[URL]] {
  
  /**
   * If there are any errors, it returns a ValidationError, but if no errors are present,
   * then it returns an Option[java.net.URL].
   */
  override def asValue(strs: Seq[String]) = strs match {
    case Seq() => Right(None)
    case Seq(str) => Right(Some(new URL(str)))
    case _ => Left(ValidationError("Expected zero or one URLs, but got many."))
  }
}