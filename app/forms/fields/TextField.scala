package forms.fields

import scala.xml.{Attribute, MetaData, Null, Text}


import forms.validators._
import forms.widgets._

abstract class BaseTextField[T](name: String)(implicit man: Manifest[T]) extends Field[T](name) {
  val minLength: Option[Int] = None
  val maxLength: Option[Int] = None
  val autocomplete: Option[Boolean] = None
  
  override def widgetAttrs(widget: Widget): MetaData = {
    val maxLengthAttr: MetaData = if (this.maxLength.isDefined && (widget.isInstanceOf[TextInput] || widget.isInstanceOf[PasswordInput])) {
      Attribute("maxlength", Text(maxLength.get.toString), Null)
    } else Null
    super.widgetAttrs(widget).append(maxLengthAttr)
  }
}

class TextField(name: String) extends BaseTextField[String](name) {
  def asValue(strs: Seq[String]): Either[ValidationError, String] = {
    strs match {
      case Seq(s) => Right(s)
      case _ => Left(ValidationError("Expected a single value; got none or many."))
    }
  }

  override def validators = TextField.minAndMaxValidators(minLength, maxLength)
}

class TextFieldOptional(name: String) extends BaseTextField[Option[String]](name) {
  def asValue(strs: Seq[String]): Either[ValidationError, Option[String]] = {
    strs match {
      case Seq() => Right(None)
      case Seq(s) => Right(Some(s))
      case _ => Left(ValidationError("Expected a single value; got multiples."))
    }
  }
  
  override def validators = OptionValidator(TextField.minAndMaxValidators(minLength, maxLength))
}

object TextField {
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