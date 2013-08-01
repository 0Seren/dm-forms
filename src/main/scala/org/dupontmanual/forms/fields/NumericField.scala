package org.dupontmanual.forms.fields

import scala.xml.Text

import org.dupontmanual.forms.widgets._
import org.dupontmanual.forms.validators._

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
  override def widget = 
    if(manifest[T] == manifest[Double] || manifest[T]==manifest[Float]) new TextInput(required, _inputType="number", _step=true)
    else new TextInput(required, _inputType = "number")
  
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
  
  override def required = false // needed because there's an extra [T] floating around in NumericField, so the
                                  // Option doesn't get picked up correctly
  
  /**
   * Sets the widget.
   */
  override def widget = new TextInput(required, _inputType = "number")
  
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
      case Some(min) => List(new MinValueValidator[T](min, 
          (x => Text(s"This value must be at least ${min}."))))
    }
    val max = maxValue match {
      case None => Nil
      case Some(max) => List(new MaxValueValidator[T](max, 
          (x => Text(s"This value must be at most ${max}."))))
    }
    min ++ max
  }
  
  /**
   * Converts a string to type T when T is an Int, Double, Long, Short, or Float.
   */
  def conversionFunction[T](implicit tag: Manifest[T]): ((String => T), (String => String)) = {
    if (manifest[T] == manifest[Int]) {
      ((s: String) => s.toInt.asInstanceOf[T], (s: String) => "This value must be a positive or negative whole number (Int).")
    } else if (manifest[T] == manifest[Double]) {
      ((s: String) => s.toDouble.asInstanceOf[T], (s: String) => "This value must be a number (Double).")
    } else if (manifest[T] == manifest[Long]) {
      ((s: String) => s.toLong.asInstanceOf[T], (s: String) => "This value must be a positive or negative whole number (Long).")
    } else if (manifest[T] == manifest[Short]) {
      ((s: String) => s.toShort.asInstanceOf[T], (s: String) => "This value must be a positive or negative whole number (Short).")
    } else if (manifest[T] == manifest[Float]) {
      ((s: String) => s.toFloat.asInstanceOf[T], (s: String) => "This value must be a number (Float).")
    } else {
      throw new Exception("Numeric field only supported for Int, Double, Long, Short, and Float.")
    }
  }
}
