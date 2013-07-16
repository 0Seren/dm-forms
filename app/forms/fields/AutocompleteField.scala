package forms.fields

import forms.validators.ValidationError
import forms.widgets.AutocompleteInput
import forms._

abstract class BaseAutocompleteField[T](name: String, list: List[Any])(implicit man: Manifest[T]) extends Field[T](name) {
  
  val uuid=java.util.UUID.randomUUID()
  
  override def widget = new AutocompleteInput(required, toJsArray(list), uuid=uuid)
  
  def toJsArray(s: List[Any]): String = {
    val strings= "[" + s.map(x => "\""+x.toString+"\"").reduce(_+","+_) + "]"
    //System.out.println(strings)
    strings
  }
}

class AutocompleteField(name: String, list: List[Any]) extends BaseAutocompleteField[String](name, list) {
  
  def asValue(s: Seq[String]): Either[ValidationError, String] = Right("")

}

class AutocompleteFieldOptional(name: String, list: List[Any]) extends BaseAutocompleteField[Option[String]](name, list) {
    def asValue(s: Seq[String]): Either[ValidationError, Option[String]] = 
      if(s.isEmpty) Right(None)
      else Right(Some(s(0)))
}