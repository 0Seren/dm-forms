package forms.validators

import Numeric._
import javax.mail.internet.InternetAddress
import javax.mail.internet.AddressException

/**
 * A Validator is a Function from T to ValidationError
 * If the value is valid, the function should return ValidationError[Nil],
 * otherwise it should return a list of messages explaining why the input
 * is invalid
 */
abstract class Validator[T] extends Function[T, ValidationError]

object Validator {
  def apply[T](f: Function[T, ValidationError]): Validator[T] = {
    new Validator[T] {
      def apply(t: T) = f(t)
    }
  }
}

object OptionValidator {
  def apply[T](validators: List[Validator[T]]): List[Validator[Option[T]]] = {
    validators.map(vdator => 
      new Validator[Option[T]] {
        def apply(ot: Option[T]) = ot match {
          case None => ValidationError(Nil)
          case Some(t) => vdator(t)
        }
      }
    )
  }
}
class MinLengthValidator(minLength: Int, msg: (String => String)) extends Validator[String] {
  def apply(str: String): ValidationError = {
    if (str.length < minLength) ValidationError(msg(str)) else ValidationError(Nil)
  }
} 

class MaxLengthValidator(maxLength: Int, msg: (String => String)) extends Validator[String] {
  def apply(str: String): ValidationError = {
    if (str.length > maxLength) ValidationError(msg(str)) else ValidationError(Nil)
  }
}

class MinValueValidator[T](minValue: T, msg: (T => String))(implicit n: Numeric[T]) extends Validator[T] {
  def apply(value: T): ValidationError = {
    if (n.lt(value, minValue)) ValidationError(msg(value)) else ValidationError(Nil)
  }
}

class MaxValueValidator[T](maxValue: T, msg: (T => String))(implicit n: Numeric[T]) extends Validator[T] {
  def apply(value: T): ValidationError = {
    if (n.gt(value, maxValue)) ValidationError(msg(value)) else ValidationError(Nil)
  }
}

object EmailValidator extends Validator[String] {
  def apply(str: String): ValidationError = {
    try {
      val emailAddress = new InternetAddress(str)
      emailAddress.validate()
      ValidationError(Nil)
    } catch {
      case e: AddressException => ValidationError("Enter a valid email address.")
    }
  }
}
