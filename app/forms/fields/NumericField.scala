package forms.fields

import forms.widgets._
import forms.validators._
import scala.reflect.runtime.universe._

// TODO: use type="number" from HTML5
/**
 * Sets some default characteristics for NumericField and NumericFieldOptional
 */
trait BaseNumericField[T] {
  
  /**
   * Sets the minValue of the field. Default None.
   */
  val minValue: Option[T] = None
  
  /**
   * Sets the maxValue of the field. Default None.
   */
  val maxValue: Option[T] = None
}

/**
 * NumericField creates a field of type number, and will return type T.
 */
class NumericField[T](name: String)(implicit n: Numeric[T], man: Manifest[T]) 
    extends Field[T](name) with BaseNumericField[T] {
  
  /**
   * Sets the widget for the NumericField.
   */
  override def widget = {
    if(typeOf[T] == typeOf[Int] || typeOf[T] == typeOf[Long] || typeOf[T] == typeOf[Byte] || typeOf[T] == typeOf[Short]) new TextInput(required, _inputType = "number")
    else new TextInput(required, _inputType = "number", _step=true)
  }
  
  /**
   * Returns a ValidationError if there are issues with compatability or is empty. Returns type T if
   * everything works correctly.
   */
  def asValue(strs: Seq[String]): Either[ValidationError, T] = {
    val (toT, errorMsg) = NumericField.conversionFunction[T]
    strs match {
      case Seq(s) => try {
        Right(toT(s.trim))
      } catch {
        case e: NumberFormatException => Left(ValidationError(errorMsg(s)))
      }
      case _ => Left(ValidationError("Expected a single value; got none or many."))
    }
  }
  
  /**
   * Adds the min and max validators to the validator list
   */
  override def validators = NumericField.minAndMaxValidators(minValue, maxValue)
}

/**
 * Creates an optional NumericField
 */
class NumericFieldOptional[T](name: String)(implicit n: Numeric[T], man: Manifest[T])
    extends Field[Option[T]](name) with BaseNumericField[T] {
  
  /**
   * Sets the widget.
   */
  override def widget = {
    if(typeOf[T] == typeOf[Int] || typeOf[T] == typeOf[Long] || typeOf[T] == typeOf[Short]) new TextInput(required, _inputType = "number")
    else new TextInput(required, _inputType = "number", _step=true)
  }
  
  /**
   * Returns an Option[T] if the input is empty or valid, and a ValidationError if the
   * input is invalid.
   */
  def asValue(strs: Seq[String]): Either[ValidationError, Option[T]] = {
    val (toT, errorMsg) = NumericField.conversionFunction[T]
    strs match {
      case Seq() => Right(None)
      case Seq(s) => try {
        Right(Some(toT(s.trim)))
      } catch {
        case e: NumberFormatException => Left(ValidationError(errorMsg(s)))
      }
      case _ => Left(ValidationError("Expected a single value, got multiples."))
    }
  }
  
  /**
   * Adds the option validator for min and max value to the list of validators.
   */
  override def validators = OptionValidator(NumericField.minAndMaxValidators(minValue, maxValue))
}

/**
 * Holds functions needed for NumericField classes.
 */
object NumericField {
  
  /**
   * Creates the min and max vaidators.
   */
  def minAndMaxValidators[T](minValue: Option[T], maxValue: Option[T])(implicit n: Numeric[T]): List[Validator[T]] = {
    val min = minValue match {
      case None => Nil
      case Some(min) => List(new MinValueValidator[T](min, (x => "This value must be at least %s.".format(min))))
    }
    val max = maxValue match {
      case None => Nil
      case Some(max) => List(new MaxValueValidator[T](max, (x => "This value must be at most %s.".format(max))))
    }
    min ++ max
  }
  
  /**
   * Converts a string to type T when T is an Int, Double, Long, Short, or Float.
   */
  def conversionFunction[T](implicit tag: TypeTag[T]): ((String => T), (String => String)) = {
    if (typeOf[T] == typeOf[Int]) {
      ((s: String) => s.toInt.asInstanceOf[T], (s: String) => "This value must be a positive or negative whole number.")
    } else if (typeOf[T] == typeOf[Double]) {
      ((s: String) => s.toDouble.asInstanceOf[T], (s: String) => "This value must be a number.")
    } else if (typeOf[T] == typeOf[Long]) {
      ((s: String) => s.toLong.asInstanceOf[T], (s: String) => "This value must be a number.")
    } else if (typeOf[T] == typeOf[Short]) {
      ((s: String) => s.toShort.asInstanceOf[T], (s: String) => "This value must be a number.")
    } else if (typeOf[T] == typeOf[Float]) {
      ((s: String) => s.toFloat.asInstanceOf[T], (s: String) => "This value must be a number.")
    } else {
      throw new Exception("Numeric field only supported for Int, Double, Long, Short, and Float.")
    }
  }
}