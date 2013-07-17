package forms.fields

import scala.xml.{Attribute, MetaData, Null, Text}


import forms.validators._
import forms.widgets._

/**
 * Creates the methods common to both TextField and TextFieldOptional
 */
abstract class BaseTextField[T](name: String)(implicit man: Manifest[T]) extends Field[T](name) {
  
  /**
   * Sets the min Length constraint.
   */
  val minLength: Option[Int] = None
  
  /**
   * Sets the max length constraint.
   */
  val maxLength: Option[Int] = None
  
  /**
   * Turns the autocomplete attribute on or off.
   */
  val autocomplete: Option[Boolean] = None
  
  /**
   * Creates new widgetAttrs for the widget.
   */
  override def widgetAttrs(widget: Widget): MetaData = {
    val maxLengthAttr: MetaData = if (this.maxLength.isDefined && (widget.isInstanceOf[TextInput] || widget.isInstanceOf[PasswordInput])) {
      Attribute("maxlength", Text(maxLength.get.toString), Null)
    } else Null
    super.widgetAttrs(widget).append(maxLengthAttr)
  }
}

/**
 * Creates a new required TextField.
 */
class TextField(name: String) extends BaseTextField[String](name) {
  
  /**
   * Returns the user's input as a String, or a ValidationError if something
   * goes wrong, or the user doesn't input any value.
   */
  def asValue(strs: Seq[String]): Either[ValidationError, String] = {
    strs match {
      case Seq(s) => Right(s)
      case _ => Left(ValidationError("Expected a single value; got none or many."))
    }
  }

  /**
   * Creates the min and max length validators.
   */
  override def validators = TextField.minAndMaxValidators(minLength, maxLength)
}

/**
 * Creates a new optional TextField.
 */
class TextFieldOptional(name: String) extends BaseTextField[Option[String]](name) {
  
  /**
   * Returns an Option[String] if there were no errors with the input,
   * and returns a ValidationError if there were errors.
   */
  def asValue(strs: Seq[String]): Either[ValidationError, Option[String]] = {
    strs match {
      case Seq() => Right(None)
      case Seq(s) => Right(Some(s))
      case _ => Left(ValidationError("Expected a single value; got multiples."))
    }
  }
  
  /**
   * Adds the optional min and max length validators to the validator list.
   */
  override def validators = OptionValidator(TextField.minAndMaxValidators(minLength, maxLength))
}

/**
 * Contains methods for use in fields.
 */
object TextField {
  
  /**
   * Creates validators for the min and max length.
   */
  def minAndMaxValidators(minLength: Option[Int], maxLength: Option[Int]): List[Validator[String]] = {
    val min = minLength match {
      case None => Nil
      case Some(min) => List(new MinLengthValidator(min, (s => "This value must have at least %d characters. (It has %d.)".format(min, s.length))))
    }
    val max = maxLength match {
      case None => Nil
      case Some(max) => List(new MaxLengthValidator(max, (s => "This value must have no more than %d characters. (It has %d.)".format(max, s.length))))
    }
    min ++ max
  }
}