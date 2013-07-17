package controllers.for_forms

import play.api.mvc.{Action, Controller}

/*
 forms._ and forms.fields._ are required for creating a generic form.
 If you want to have more options, forms.widgets._, foms.validators._, 
 and/or scala.xml... may be needed.
*/

import forms._
import forms.fields._
import forms.widgets._
import forms.validators._
import scala.xml.NodeSeq.seqToNodeSeq


import com.google.inject.{ Inject, Singleton }
import play.api.templates.Html

/**
This is an example of how to implement a form in a controller.
*/
object App extends Controller {
  
  /**
    Creates a new form object that includes the wanted fields.
  */
  object FormTests extends Form {
    
    /**
    These vals are the different Fields that you want to include in the form
    in no particular order. See forms.fields.<name of field> for specific 
    details about each of the fields.
    */
    val BooleanField = new BooleanField("Boolean", "THIS IS Forks High School, home of the SPARTANS")
    
    /**
    Creates a new ChoiceFieldOptional
    */
    val ChoiceField = new ChoiceFieldOptional("Choice", List(("hi", "hi"), ("bye", "bye")))
    
    /**
     * Creates a new DateFieldOptional
     */
    val DateField = new DateFieldOptional("Date")
    
    /**
     * Creates a new TimeFieldOptional
     */
    val TimeField = new TimeFieldOptional("Time")    
    
    /**
     * Creates a new DateTimeFieldOptional
     */
    val TimestampField = new DateTimeFieldOptional("Timestamp")
    
    /**
     * Creates a new EmailFieldOptional
     */
    val EmailField = new EmailFieldOptional("Email")
    
    /**
     * Creates a new NumericFieldOptional
     */
    val NumericField = new NumericFieldOptional[Double]("Double")
    
    /**
     * Creates a new PasswordFieldOptional
     */
    val PasswordField = new PasswordFieldOptional("Password")
    
    /**
     * Creates a new TextFieldOptional
     */
    val TextField = new TextFieldOptional("Text")
    
    /**
     * Creates a new UrlFieldOptional
     */
    val UrlField = new UrlFieldOptional("Url")
    
    /**
     * Creates a new PhoneFieldOptional
     */
    val PhoneField = new PhoneFieldOptional("Phone")
    
    /**
     * Just a List[String] for use in AutocompleteField
     */
    val listOfSpectopers = List("Allen", "Austin", "Angular", "Anteater", "Awesomeness", "Anti-climatic", "Audi S8", "Quadralateral", "Blagoshpere", "Wisconsin", "Zach", "John", "Mack", "Ava", "Sam Fu", "Jim", "Mr. O'Bryan", "Eliza", "Xavier", "Zander Smith", "Others")
    
    /**
     * Creates a new AutocompleteFieldOptional
     */
    val ACField = new AutocompleteFieldOptional("AC", listOfSpectopers)
    
    /**
     * Creates a new CheckboxFieldOptional
     */
    val Checkboxo = new CheckboxFieldOptional("Checkboxes", List(("car", 11) , ("van", 12) , ("truck", 13)))
    
    /**
     * Creates a new RadioField: Not Optional because there are no optional radio fields
     */
    val RadioR = new RadioField("Radio", List(("cat",11),("dog",12),("mouse",13), ("Bird", "BIRD BIRD BIRD! BIRD IS THE WORD."), ("Turtle", listOfSpectopers)))
    
    /**
     * Creates a new CheckboxFieldOptional
     */
    val MultChoiceField = new CheckboxFieldOptional("Mult Choice", List(("UK", "Kentucky"),("University of Illinois", "Illinois"),("Wash U", "Missouri"),("MIT", "Massachucets")), useSelectInputMult = true)
    
    /**
     * Creates a new FileFieldOptional
     */
    val FileField = new FileFieldOptional("File")

    /**
    This editedTextField shows an example of how to override just some 
    options/methods that are present in a field.
    */
    val editedTextField = new TextFieldOptional("edited") {
      override def widget = new TextInput(required)

      override def helpText = Some(<p>Please input "lolCats" for true</p>)

      override def asValue(s: Seq[String]): Either[ValidationError, Option[String]] = {
        s match {
          case Seq() => Right(None)
          case Seq(str) => if (str == "lolCats") Right(Some("true")) else Left(ValidationError("Must be \"lolCats\" or empty. The rest of this message is to show how these errors will behave with extremely long error messages because those may be used for some reason."))
          case _ => Left(ValidationError("Expected a single value or none, got multiples."))
        }
      }
    }

    /**
    Of type List[Field] and includes the fields you want to 
    put on your webpage in the order you want them to appear.
    */
    val fields = List(editedTextField, RadioR, BooleanField, FileField, MultChoiceField, Checkboxo, ACField, ChoiceField, DateField, TimeField, TimestampField, EmailField, NumericField, PasswordField, PhoneField, TextField, UrlField)

    /*
    These are just some methods or options in Form that are being overriden
    so as to change how the Form looks. Not required.
    */
 
    /**
     * Example of overriding some options of a form.
     */
    override def prefix: Option[String] = None
    
    /**
     * Example of overriding some options of a form.
     */
    override def submitText = "Submit"
    
    /**
     * Example of overriding some options of a form.
     */  
    override def includeCancel = true
    
    /**
     * Example of overriding some options of a form.
     */
    override def cancelText = "Cancel"

  }
  
  /**
  Method formTest wraps the form in a Binding, and then sends it to the 
  templates page (or views page) to be displayed. Binding is required for
  the input of the user to be processed.
  */
  def formTest() = Action { implicit req =>
    Ok(Html(templates.for_forms.Tester(Binding(FormTests)))) 
  }

  /**
  Method formTestP processes the returned Binding from the form submission via 
  POST and can either be an InvalidBinding (in the case of form errors) or a 
  ValidBinding (in the case there were no errors). They can be handled however
  you wish. In this case an InvalidBinding would be returned to the webpage for
  the user to attempt to correct the mistakes, and, in the case of a ValidBinding,
  will grab the returned value (see forms.fields._.asValue to see what is returned 
  for each field) of each field and displays the output on a webpage.
  */
  def formTestP() = Action { implicit req =>
    Binding(FormTests, req) match {
      case ib: InvalidBinding => Ok(Html(templates.for_forms.Tester(ib)))
      case vb: ValidBinding => {
        val TheChoice = vb.valueOf(FormTests.ChoiceField)
        val TheDate = vb.valueOf(FormTests.DateField)
        val TheTime = vb.valueOf(FormTests.TimeField)
        val TheTimestamp = vb.valueOf(FormTests.TimestampField)
        val TheEmail = vb.valueOf(FormTests.EmailField)
        val TheNumeric = vb.valueOf(FormTests.NumericField)
        val ThePassword = vb.valueOf(FormTests.PasswordField)
        val TheText = vb.valueOf(FormTests.TextField)
        val TheUrl = vb.valueOf(FormTests.UrlField)
        val TheEdited = vb.valueOf(FormTests.editedTextField)
        val ThePhone = vb.valueOf(FormTests.PhoneField)
        val TheCheckboxO = vb.valueOf(FormTests.Checkboxo)
        val TheRadioR = vb.valueOf(FormTests.RadioR)
        val TheChoiceMult = vb.valueOf(FormTests.MultChoiceField)
        val TheFile = vb.valueOf(FormTests.FileField)
        val TheAutoComplete = vb.valueOf(FormTests.ACField)
        val TheBoolean = vb.valueOf(FormTests.BooleanField)
        val listOfStuff = List(("Radio", TheRadioR.toString),("Boolean Field", TheBoolean.toString),("File", TheFile.toString),("Choice Field Mult", TheChoiceMult.toString),("Checkbox Optional", TheCheckboxO.toString),("AutoComplete Field", TheAutoComplete.toString),("Choice Field", TheChoice.toString), ("Date Field", TheDate.toString), ("Time Field", TheTime.toString), ("Timestamp Field", TheTimestamp.toString), ("Email Field", TheEmail.toString), ("NumericField", TheNumeric.toString), ("Password Field", ThePassword.toString), ("Phone Field", ThePhone.toString), ("Text Field", TheText.toString), ("Url Field", TheUrl.toString), ("Edited Field", TheEdited.toString))

        Ok(Html(templates.for_forms.Results(listOfStuff)))
      }
    }
  }
}

