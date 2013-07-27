package org.dupontmanual.forms.fields

import scala.reflect.runtime.universe.TypeTag

import org.dupontmanual.forms.widgets._
import org.dupontmanual.forms.validators.ValidationError

class RadioField[T](name: String, choices: List[(String, T)])(implicit tag: TypeTag[T])
    extends ChoiceField[T](name, choices) {
  override def widget: Widget = new RadioInput(choices.map(_._1))
}