package org.dupontmanual.forms.validators

import scala.xml.NodeSeq
import javax.mail.internet.InternetAddress
import javax.mail.internet.AddressException

/** A `Validator` is a `Function` from `T` to `ValidationError`.
  * If a value of type `T` is valid, the function should return 
  * `ValidationError(Nil)`, otherwise it should return a 
  * `ValidationError` containing a list of messages explaining
  * why the input is invalid.
  */
abstract class Validator[T] extends Function[T, ValidationError]

/** contains a convenience method to convert a function 
 *  into a `Validator`
  */
object Validator {
  def apply[T](f: Function[T, ValidationError]): Validator[T] = {
    new Validator[T] {
      def apply(t: T) = f(t)
    }
  }
}

/** contains a convenience method to convert a list of `Validator[T]`s
 *  into a list of `Validator[Option[T]]`s. These new validators succeed
 *  for `None`, but call the original validators on the value contained
 *  in the `Option` if it has one.
  */
object OptionValidator {
  def apply[T](validators: List[Validator[T]]): List[Validator[Option[T]]] = {
    validators.map(vdator =>
      new Validator[Option[T]] {
        def apply(ot: Option[T]) = ot match {
          case None => ValidationError(Nil)
          case Some(t) => vdator(t)
        }
      })
  }
}

/** A validator that checks if a `String` is at least a certain length. The function
 *  is called on the `String` being validated to return an error message.
  */
class MinLengthValidator(minLength: Int, msg: (String => NodeSeq)) extends Validator[String] {
  def apply(str: String): ValidationError = {
    if (str.length < minLength) ValidationError(msg(str)) else ValidationError(Nil)
  }
}

/** A validator that checks if a `String` is at most a certain length. The function
 *  is called on the `String` being validated to return an error message.
  */
class MaxLengthValidator(maxLength: Int, msg: (String => NodeSeq)) extends Validator[String] {
  def apply(str: String): ValidationError = {
    if (str.length > maxLength) ValidationError(msg(str)) else ValidationError(Nil)
  }
}

/** Creates a min value validator.
  */
class MinValueValidator[T](minValue: T, msg: (T => NodeSeq))(implicit n: Numeric[T]) extends Validator[T] {
  def apply(value: T): ValidationError = {
    if (n.lt(value, minValue)) ValidationError(msg(value)) else ValidationError(Nil)
  }
}

/** Creates a max value validator.
  */
class MaxValueValidator[T](maxValue: T, msg: (T => NodeSeq))(implicit n: Numeric[T]) extends Validator[T] {
  def apply(value: T): ValidationError = {
    if (n.gt(value, maxValue)) ValidationError(msg(value)) else ValidationError(Nil)
  }
}

/** Creates an email validator.
  */
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
