package forms.fields

import forms.widgets._
import forms.validators.ValidationError
import play.api.mvc.MultipartFormData.FilePart


/**
 * Sets methods common o FileField and FileFieldOptional.
 */
abstract class BaseFileField[T](name: String)(implicit man: Manifest[T]) extends Field[T](name){
  
  /**
   * Sets the widget for both FileField and FileField Optional.
   */
  override def widget = new FileInput(required)
  
  /**
   * This isn't used for FileFields and just returns a ValidationError.
   */
  def asValue(s: Seq[String]): Either[ValidationError, T] = Left(ValidationError("How did you even???"))
  
  /**
   * This returns a Seq[FilePart[_]].
   */
  def validate(files: Seq[FilePart[_]]): Either[ValidationError, Seq[FilePart[_]]] = Right(files)
  
  /**
   * Creates the cleanFiles abstract def.
   */
  def cleanFiles(files: Seq[FilePart[_]]): Either[ValidationError, T]
  
  /**
   * Checks to see if the input fits the required requirements.
   */
  def checkFileRequired(files: Seq[FilePart[_]]): Either[ValidationError, Seq[FilePart[_]]]={
    files.filter(_.key == this.name) match{
      case Seq() => if(this.required) Left(ValidationError(errorMessages("required")))
    		  		else Right(Seq())
      case Seq(fs@_*) => Right(fs)
    }
  }
}

/**
 * Creates a new FileField.
 */
class FileField(name: String) extends BaseFileField[Seq[FilePart[_]]](name){
    
    /**
     * Returns a Seq[FilePart[_]] if a valid file is submitted and a ValidationError
     * if an invalid file or no file is submitted.
     */
	def cleanFiles(files: Seq[FilePart[_]]): Either[ValidationError, Seq[FilePart[_]]]={
    	checkFileRequired(files).fold(
        Left(_), validate(_))
    }
}

/**
 * Creates an optional FileField.
 */
class FileFieldOptional(name: String) extends BaseFileField[Option[Seq[FilePart[_]]]](name) {
  
  /**
   * Returns an Option[Seq[FilePart[_]]] if no file or a valid file is submitted. Returns
   * a ValidationError if an invalid file is submitted.
   */
  def cleanFiles(files: Seq[FilePart[_]]): Either[ValidationError, Option[Seq[FilePart[_]]]]={
   	val maybe = checkFileRequired(files).fold(
    Left(_), validate(_))
    if(maybe.isLeft) Left(maybe.left.get)
    else if(maybe.right.get.isEmpty) Right(None)
    else Right(Some(maybe.right.get))
  }
}