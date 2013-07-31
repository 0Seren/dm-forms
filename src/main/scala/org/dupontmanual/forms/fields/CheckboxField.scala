package org.dupontmanual.forms.fields

import org.dupontmanual.forms.widgets._
import org.dupontmanual.forms.validators.ValidationError

/** A required set of checkboxes where at least one box has to be checked. name: String is the label for the
  * set of checkboxes, and choices: List[(String, T)] determines what is displayed by each checkbox (choices._1)
  * and what should be returned should that checkbox be checked (choices._2).
  */
class CheckboxField[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) 
    extends ChoiceField[T](name, choices) {
  override def required = true
  override def widget: Widget = new CheckboxInput(required, choices.map(_._1))
}

/** An optional set of checkboxes. name: String is the label for the set of checkboxes,
  * and choices: List[(String, T)] determines what is displayed by each checkbox (choices._1)
  * and what should be returned should that checkbox be checked (choices._2).
  */
class CheckboxFieldOptional[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T]) 
    extends ChoiceFieldOptional[T](name, choices) {
  override def required = false
  override def widget: Widget = new CheckboxInput(required, choices.map(_._1))
}
