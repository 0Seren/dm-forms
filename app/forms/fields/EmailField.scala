package forms.fields
import forms.validators._
import forms.widgets.TextInput

trait BaseEmailField[T] extends Field[T] {
  override def widget = new TextInput(required, _inputType="email")
}

class EmailField(name: String) extends TextField(name) with BaseEmailField[String] {
  override def validators = super.validators ++ List(EmailValidator) 
  override def asValue(strs: Seq[String]) = super.asValue(strs.map(_.trim))
}

class EmailFieldOptional(name: String) extends TextFieldOptional(name) with BaseEmailField[Option[String]] {
  override def validators = super.validators ++ OptionValidator(List(EmailValidator))
  override def asValue(strs: Seq[String]) = super.asValue(strs.map(_.trim))
}

