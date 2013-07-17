package forms.fields

import forms.validators.ValidationError
import forms.widgets.AutocompleteInput
import forms._

/**
 * Sets the widget and a helper method for AutocompleteFied and AutocompleteFieldOptional.
 */
abstract class BaseAutocompleteField[T](name: String, list: List[Any])(implicit man: Manifest[T]) extends Field[T](name) {
  
  /**
   * Sets the widget for both AutocompleteField and AutocompleteFeildOptional.
   */
  override def widget = new AutocompleteInput(required, toJsArray(list))
  
  /**
   * Is used to change the list[Any] to the correct format. 
   */
  def toJsArray(s: List[Any]): String = {
    "[" + s.map(x => "\""+x.toString+"\"").reduce(_+","+_) + "]"
  }
}

/**
 * AutocompleteField takes a name, which is used for the label of the field, and a list[Any] which is used
 * to create the options from which the user chooses. AutocompleteField returns either a 
 * ValidationError or the user's input as a string.
 */
class AutocompleteField(name: String, list: List[Any]) extends BaseAutocompleteField[String](name, list) {
  
  /**
   * Throws a ValidationError if field is empty or returns the user's input if as a string if filled. 
   */
  def asValue(s: Seq[String]): Either[ValidationError, String] = 
    if(s.isEmpty) Left(ValidationError("This field is required."))
    else Right(s(0))

}

/**
 * AutocompleteFieldOptional takes a name, which is used for the label of the field, and a 
 * list[Any] which is used to create the options from which the user chooses. AutocompleteFieldOptional 
 * returns an Option[String].
 */
class AutocompleteFieldOptional(name: String, list: List[Any]) extends BaseAutocompleteField[Option[String]](name, list) {
  
   /**
    * Returns None if field is empty or Some(userInput: String) if filled.
    */
    def asValue(s: Seq[String]): Either[ValidationError, Option[String]] = 
      if(s.isEmpty) Right(None)
      else Right(Some(s(0)))
}