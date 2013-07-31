package org.dupontmanual.forms.fields

import org.dupontmanual.forms.widgets._
import org.dupontmanual.forms.validators.ValidationError

class RadioField[T](name: String, choices: List[(String, T)])(implicit man: Manifest[T])
    extends ChoiceField[T](name, choices) {
  override def widget: Widget = new RadioInput(choices.map(_._1))
}