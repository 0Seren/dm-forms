package org.dupontmanual.forms.fields

import org.dupontmanual.forms.validators.ValidationError
import org.dupontmanual.forms.widgets.SelectInput
import org.dupontmanual.forms.widgets.Widget

/** `BaseChoiceField` takes a `name: String`, `choices: List[(String, T)]`,
 *  and `allowsMultiple: Boolean`, which defaults to `false`. 
 *  The `String` in each tuple in `choices` is what is displayed to the
 *  user, and `T` is the object you would like returned should the user
 *  select that option. (The `T`s should be distinct, since each `T` is
 *  translated into the index of its corresponding `String` using `indexOf`,
 *  so two `T`s that are `==` could be swapped for one another, even if the
 *  `String`s describing them are different. `List[T]` or `ValidationError`
 *  will be returned. 
  */
abstract class BaseChoiceField[T, U](name: String, choices: List[(String, T)], allowMultiple: Boolean = false)(implicit man: Manifest[U])
    extends Field[U](name) {
  /** Returns the correct indexes for the selected values.
    */
  def toStringSeq(value: List[T]): Seq[String] = {
    val objs: List[T] = choices.map(_._2)
    def listOfIndices(elems: List[T]): List[Int] = elems match {
      case Nil => Nil
      case elem :: rst => objs.indexOf(elem) :: listOfIndices(rst)
    }
    listOfIndices(value).map(_.toString)
  }

  /** Given a list of indices (as `String`s), returns a list of the corresponding `T`s.  */
  def toValue(s: Seq[String]): Either[ValidationError, List[T]] = {
    try {
      val listOfIndices = s.map(_.toInt) 
      if (listOfIndices.contains(-1)) Left(ValidationError(errorMessages("unexpected")))
      else if (this.required && listOfIndices.isEmpty) Left(ValidationError(errorMessages("required")))
      else if (listOfIndices.length > 1 && !allowMultiple) Left(ValidationError(errorMessages("multiple")))
      else {
        Right(listOfIndices.map(index => choices(index)._2).toList) //returns the objects
      }
    } catch {
      case i: IndexOutOfBoundsException => Left(ValidationError(errorMessages("unexpected")))
      case n: NumberFormatException => Left(ValidationError(errorMessages("unexpected")))
      case e: Throwable => Left(ValidationError("Invalid Input: " + e.toString))
    }
  }
  
  override def widget: Widget = new SelectInput(required, choices.map(_._1), allowMultiple = allowMultiple)
}

/** A required set of checkboxes where at least one box has to be checked. name: String is the label for the
  * set of checkboxes, and choices: List[(String, T)] determines what is displayed by each checkbox (choices._1)
  * and what should be returned should that checkbox be checked (choices._2).
  */
class ChoiceField[T](name: String, choices: List[(String, T)], allowMultiple: Boolean = false)(implicit man: Manifest[T]) 
    extends BaseChoiceField[T, T](name, choices) {
  override def asStringSeq(value: Option[T]): Seq[String] = {
    super.toStringSeq(value.toList)
  }
  
  override def asValue(s: Seq[String]): Either[ValidationError, T] = {
    super.toValue(s).fold(Left(_), vals => Right(vals(0)))
  }
}

/** An optional set of checkboxes. name: String is the label for the set of checkboxes,
  * and choices: List[(String, T)] determines what is displayed by each checkbox (choices._1)
  * and what should be returned should that checkbox be checked (choices._2).
  */
class ChoiceFieldOptional[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) 
    extends BaseChoiceField[T, Option[T]](name, choices) {
  override def asStringSeq(value: Option[Option[T]]): Seq[String] = value match {
    case Some(Some(t)) => super.toStringSeq(List(t))
    case _ => super.toStringSeq(Nil)
  }
  
  override def asValue(s: Seq[String]): Either[ValidationError, Option[T]] = {
    super.toValue(s).fold(Left(_), vals => if (vals.isEmpty) Right(None) else Right(Some(vals(0))))
  }
}

class ChoiceFieldMultiple[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T])
    extends BaseChoiceField[T, List[T]](name, choices, allowMultiple = true) {
  override def asStringSeq(value: Option[List[T]]): Seq[String] = super.toStringSeq(value.getOrElse(Nil))
  
  override def asValue(s: Seq[String]): Either[ValidationError, List[T]] = super.toValue(s)
}

class ChoiceFieldMultipleOptional[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T])
    extends BaseChoiceField[T, List[T]](name, choices, allowMultiple = true) {
  override def required = false
  
  override def asStringSeq(value: Option[List[T]]): Seq[String] = super.toStringSeq(value.getOrElse(Nil))
  
  override def asValue(s: Seq[String]): Either[ValidationError, List[T]] = super.toValue(s)
}
