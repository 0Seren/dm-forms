package controllers.for_forms

import play.api.mvc.{Action, Controller}
import forms._
import forms.fields._
import forms.widgets._
import forms.validators._
import scala.xml.NodeSeq.seqToNodeSeq
import com.google.inject.{ Inject, Singleton }
import play.api.templates.Html

object App extends Controller {

  object FormTests extends Form {
    val BooleanField = new BooleanField("Boolean", "THIS IS Forks High School, home of the SPARTANS")
    val ChoiceField = new ChoiceFieldOptional("Choice", List(("hi", "hi"), ("bye", "bye")))
    val DateField = new DateFieldOptional("Date")
    val TimeField = new TimeFieldOptional("Time")    
    val TimestampField = new DateTimeFieldOptional("Timestamp")
    val EmailField = new EmailFieldOptional("Email")
    val NumericField = new NumericFieldOptional[Double]("Double")
    val PasswordField = new PasswordFieldOptional("Password")
    val TextField = new TextFieldOptional("Text")
    val UrlField = new UrlFieldOptional("Url")
    val PhoneField = new PhoneFieldOptional("Phone")
    val listOfSpectopers = List("Allen", "Quadralateral", "Blagoshpere", "Wisconsin", "Zach", "John", "Mack", "Ava", "Sam Fu", "Jim", "Mr. O'Bryan", "Eliza", "Xavier", "Zander Smith", "Others")
    val ACField = new AutocompleteFieldOptional("AC", listOfSpectopers)
    val Checkboxo = new CheckboxFieldOptional("Checkboxes", List(("car", 11) , ("van", 12) , ("truck", 13)))
    val RadioR = new RadioField("Radio", List(("cat",11),("dog",12),("mouse",13), ("Bird", "BIRD BIRD BIRD! BIRD IS THE WORD."), ("Turtle", listOfSpectopers)))
    val MultChoiceField = new CheckboxFieldOptional("Mult Choice", List(("UK", "Kentucky"),("University of Illinois", "Illinois"),("Wash U", "Missouri"),("MIT", "Massachucets")), useSelectInputMult = true)
    val FileField = new FileFieldOptional("File")

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

    val fields = List(editedTextField, RadioR, BooleanField, FileField, MultChoiceField, Checkboxo, ACField, ChoiceField, DateField, TimeField, TimestampField, EmailField, NumericField, PasswordField, PhoneField, TextField, UrlField)

    override def prefix: Option[String] = None
    override def submitText = "Submit"
    override def includeCancel = true
    override def cancelText = "Cancel"

  }
  
  def formTest() = Action { implicit req =>
    Ok(Html(templates.for_forms.Tester(Binding(FormTests)))) 
  }

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

