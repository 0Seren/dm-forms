package forms.fields

import forms.widgets._
import forms.validators._
import scala.reflect.runtime.universe._

// TODO: use type="number" from HTML5
trait BaseNumericField[T] {
  val minValue: Option[T] = None
  val maxValue: Option[T] = None
}

class NumericField[T](name: String)(implicit n: Numeric[T], man: Manifest[T]) 
    extends Field[T](name) with BaseNumericField[T] {
  override def widget = {
    if(typeOf[T] == typeOf[Int] || typeOf[T] == typeOf[Long] || typeOf[T] == typeOf[Byte] || typeOf[T] == typeOf[Short]) new TextInput(required, _inputType = "number")
    else new TextInput(required, _inputType = "number", _step=true)
  }
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
  
  override def validators = NumericField.minAndMaxValidators(minValue, maxValue)
}

class NumericFieldOptional[T](name: String)(implicit n: Numeric[T], man: Manifest[T])
    extends Field[Option[T]](name) with BaseNumericField[T] {
  override def required = false
  override def widget = {
    if(typeOf[T] == typeOf[Int] || typeOf[T] == typeOf[Long] || typeOf[T] == typeOf[Byte] || typeOf[T] == typeOf[Short]) new TextInput(required, _inputType = "number")
    else new TextInput(required, _inputType = "number", _step=true)
  }
  
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
  
  override def validators = OptionValidator(NumericField.minAndMaxValidators(minValue, maxValue))
}

object NumericField {
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
  
  def conversionFunction[T](implicit tag: TypeTag[T]): ((String => T), (String => String)) = {
    if (typeOf[T] == typeOf[Int]) {
      ((s: String) => s.toInt.asInstanceOf[T], (s: String) => "This value must be a positive or negative whole number.")
    } else if (typeOf[T] == typeOf[Double]) {
      ((s: String) => s.toDouble.asInstanceOf[T], (s: String) => "This value must be a number.")
    } else {
      throw new Exception("Numeric field only supported for Int and Double.")
    }
  }
}