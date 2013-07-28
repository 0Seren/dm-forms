package org.dupontmanual.forms.fields

import play.api.libs.json.{ Json, JsArray }

import org.dupontmanual.forms.validators.ValidationError
import org.dupontmanual.forms.widgets.AutocompleteInput
import org.dupontmanual.forms._

/**
 * Sets the widget and a helper method for AutocompleteFied and AutocompleteFieldOptional.
 */
abstract class BaseAutocompleteField[T](name: String, list: List[String])(implicit man: Manifest[T]) extends Field[T](name) {
  
  /**
   * Sets the widget for both AutocompleteField and AutocompleteFeildOptional.
   */
  override def widget = new AutocompleteInput(required, new JsArray(list.map(Json.toJson(_))))
}

/**
 * AutocompleteField takes a name, which is used for the label of the field, and a list[Any] which is used
 * to create the options from which the user chooses. AutocompleteField returns either a 
 * ValidationError or the user's input as a string.
 */
class AutocompleteField(name: String, list: List[String]) extends BaseAutocompleteField[String](name, list) {
  
  /**
   * Throws a ValidationError if field is empty or returns the user's input if as a string if filled. 
   */
  def asValue(s: Seq[String]): Either[ValidationError, String] = 
    if(s.isEmpty) Left(ValidationError(errorMessages("required")))
    else Right(s(0))

}

/**
 * AutocompleteFieldOptional takes a name, which is used for the label of the field, and a 
 * list[Any] which is used to create the options from which the user chooses. AutocompleteFieldOptional 
 * returns an Option[String].
 */
class AutocompleteFieldOptional(name: String, list: List[String]) extends BaseAutocompleteField[Option[String]](name, list) {
  
   /**
    * Returns None if field is empty or Some(userInput: String) if filled.
    */
    def asValue(s: Seq[String]): Either[ValidationError, Option[String]] = 
      if(s.isEmpty) Right(None)
      else Right(Some(s(0)))
}
