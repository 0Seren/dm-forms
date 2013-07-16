package forms.fields

import forms.validators.ValidationError
import forms.widgets.PasswordInput

class PasswordField(name: String) extends TextField(name) {
  override def widget = new PasswordInput(this.required)
}

class PasswordFieldOptional(name: String) extends TextFieldOptional(name) {
  override def widget = new PasswordInput(required=false)
}