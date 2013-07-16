package forms

import scala.xml.{Attribute, Null, Text}

import org.scalatest.FunSuite
import fields._
import validators._
import forms.widgets._

class TestFields extends FunSuite {
  test("1. default TextField") {
    val f = new TextField("default")
    assert(f.clean("1") === Right("1"))
    assert(f.clean("hello") === Right("hello"))
    assert(f.clean("") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean(Nil) === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.maxLength === None)
    assert(f.minLength === None)
  }
  
  test("2. optional TextField") {
    val f = new TextFieldOptional("optional")
    assert(f.clean("1") === Right(Some("1")))
    assert(f.clean("hello") === Right(Some("hello")))
    assert(f.clean("") === Right(None))
    assert(f.clean(Nil) === Right(None))
    assert(f.maxLength === None)
    assert(f.minLength === None)
  }
  
  test("3. optional TextField with max length") {
    val f = new TextFieldOptional("optionalMaxLength") {
      override val maxLength = Some(10)
    }
    assert(f.clean("12345") === Right(Some("12345")))
    assert(f.clean("1234567890") === Right(Some("1234567890")))
    assert(f.clean("") === Right(None))
    assert(f.clean(Nil) === Right(None))
    assert(f.clean("12345678901") === Left(ValidationError("This value must have no more than 10 characters. (It has 11.)")))
    assert(f.maxLength === Some(10))
    assert(f.minLength === None)
  }
  
  test("4. optional TextField with min length") {
    val f = new TextFieldOptional("optionalMinLength") {
      override val minLength = Some(10)
    }
    assert(f.clean("") === Right(None))
    assert(f.clean("12345") === Left(ValidationError("This value must have at least 10 characters. (It has 5.)")))
    assert(f.clean("1234567890") === Right(Some("1234567890")))
    assert(f.clean("1234567890a") === Right(Some("1234567890a")))
    assert(f.maxLength === None)
    assert(f.minLength === Some(10))
  }
  
  test("5. required TextField with min length") {
    val f = new TextField("requiredMinLength") {
      override val minLength = Some(10)
    }
    assert(f.clean("") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("123456") === Left(ValidationError("This value must have at least 10 characters. (It has 6.)")))
    assert(f.clean("1234567890") === Right("1234567890"))
    assert(f.clean("1234567890a") === Right("1234567890a"))
    assert(f.maxLength === None)
    assert(f.minLength === Some(10))
  }
  
  test("TextField widgetAttrs") {
    val f1 = new TextField("attrs")
    assert(f1.widgetAttrs(new TextInput(f1.required)) === Null)
    val f2 = new TextField("maxAttr") {
      override val maxLength = Some(10)
    }
    assert(f2.widgetAttrs(new HiddenInput(f2.required)) === Null)
    assert(f2.widgetAttrs(new TextInput(f2.required)) === Attribute("maxlength", Text("10"), Null))
    assert(f2.widgetAttrs(new PasswordInput(f2.required)) === Attribute("maxlength", Text("10"), Null))
  }
  
  test("1. NumericField[Int]") {
    val f = new NumericField[Int]("int")
    assert(f.clean("") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean(Nil) === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("1") === Right(1))
    assert(f.clean("23") === Right(23))
    assert(f.clean("a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("3.14") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("1 ") === Right(1))
    assert(f.clean(" 2") === Right(2))
    assert(f.clean(" 3 ") === Right(3))
    assert(f.clean("4a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.maxValue === None)
    assert(f.minValue === None)
  }
  
  test("2. optional NumericField[Int]") {
    val f = new NumericFieldOptional[Int]("optionalInt")
    assert(f.clean("") === Right(None))
    assert(f.clean(Nil) === Right(None))
    assert(f.clean("1") === Right(Some(1)))
    assert(f.clean("23") === Right(Some(23)))
    assert(f.clean("a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("3.14") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("1 ") === Right(Some(1)))
    assert(f.clean(" 2") === Right(Some(2)))
    assert(f.clean(" 3 ") === Right(Some(3)))
    assert(f.clean("4a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.maxValue === None)
    assert(f.minValue === None)
  }

  test("3. required NumericField[Int] with max value") {
    val f = new NumericField[Int]("requiredIntMaxValue") {
      override val maxValue = Some(23)
    }
    assert(f.clean("") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean(Nil) === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("1") === Right(1))
    assert(f.clean("23") === Right(23))
    assert(f.clean("24") === Left(ValidationError("This value must be at most 23.")))
    assert(f.clean("a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("3.14") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("1 ") === Right(1))
    assert(f.clean(" 2") === Right(2))
    assert(f.clean(" 3 ") === Right(3))
    assert(f.clean("4a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.maxValue === Some(23))
    assert(f.minValue === None)
  }
  
  test("4. required NumericField[Int] with min value") {
    val f = new NumericField[Int]("requiredIntMinValue") { 
      override val minValue = Some(3)
    }
    assert(f.clean("") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean(Nil) === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("2") === Left(ValidationError("This value must be at least 3.")))
    assert(f.clean("23") === Right(23))
    assert(f.clean("3") === Right(3))
    assert(f.clean("a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("3.14") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("5 ") === Right(5))
    assert(f.clean(" 8") === Right(8))
    assert(f.clean(" 3 ") === Right(3))
    assert(f.clean("4a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.maxValue === None)
    assert(f.minValue === Some(3))
  }
  
  test("5. required NumericField[Int] with both min and max values") {
    val f = new NumericField[Int]("requiredIntMinAndMax") {
      override val minValue = Some(-3)
      override val maxValue=Some(3)
    }
    assert(f.clean("") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean(Nil) === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("-4") === Left(ValidationError("This value must be at least -3.")))
    assert(f.clean("-3") === Right(-3))
    assert(f.clean("0") === Right(0))
    assert(f.clean("3") === Right(3))
    assert(f.clean("4") === Left(ValidationError("This value must be at most 3.")))
    assert(f.clean("a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("3.14") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("-1 ") === Right(-1))
    assert(f.clean(" 2") === Right(2))
    assert(f.clean(" -2 ") === Right(-2))
    assert(f.clean("4a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.maxValue === Some(3))
    assert(f.minValue === Some(-3))   
  }
  
  test("6. optional NumericField[Int] with both min and max values") {
    val f = new NumericFieldOptional[Int]("optionalIntMinAndMax") {
      override val minValue = Some(-3)
      override val maxValue = Some(3)
    }
    assert(f.clean("") === Right(None))
    assert(f.clean(Nil) === Right(None))
    assert(f.clean("-4") === Left(ValidationError("This value must be at least -3.")))
    assert(f.clean("-3") === Right(Some(-3)))
    assert(f.clean("0") === Right(Some(0)))
    assert(f.clean("3") === Right(Some(3)))
    assert(f.clean("4") === Left(ValidationError("This value must be at most 3.")))
    assert(f.clean("a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("3.14") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.clean("-1 ") === Right(Some(-1)))
    assert(f.clean(" 2") === Right(Some(2)))
    assert(f.clean(" -2 ") === Right(Some(-2)))
    assert(f.clean("    ") === Right(None))
    assert(f.clean("4a") === Left(ValidationError("This value must be a positive or negative whole number.")))
    assert(f.maxValue === Some(3))
    assert(f.minValue === Some(-3))   
  }
  
  test("1. required NumericField[Double]") {
    val f = new NumericField[Double]("double")
    assert(f.clean("") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean(Nil) === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("-4") === Right(-4.0))
    assert(f.clean("-3") === Right(-3.0))
    assert(f.clean("0") === Right(0.0))
    assert(f.clean("a") === Left(ValidationError("This value must be a number.")))
    assert(f.clean("3.14") === Right(3.14))
    assert(f.clean("-1.5 ") === Right(-1.5))
    assert(f.clean(" 2.5") === Right(2.5))
    assert(f.clean(" -2.5 ") === Right(-2.5))
    assert(f.clean("    ") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("4a") === Left(ValidationError("This value must be a number.")))
    assert(f.maxValue === None)
    assert(f.minValue === None)
  }
  
  test("2. optional NumericField[Double]") {
    val f = new NumericFieldOptional[Double]("optionalDouble")
    assert(f.clean("") === Right(None))
    assert(f.clean(Nil) === Right(None))
    assert(f.clean("-4") === Right(Some(-4.0)))
    assert(f.clean("-3") === Right(Some(-3.0)))
    assert(f.clean("0") === Right(Some(0.0)))
    assert(f.clean("a") === Left(ValidationError("This value must be a number.")))
    assert(f.clean("3.14") === Right(Some(3.14)))
    assert(f.clean("-1.5 ") === Right(Some(-1.5)))
    assert(f.clean(" 2.5") === Right(Some(2.5)))
    assert(f.clean(" -2.5 ") === Right(Some(-2.5)))
    assert(f.clean("    ") === Right(None))
    assert(f.clean("4a") === Left(ValidationError("This value must be a number.")))
    assert(f.maxValue === None)
    assert(f.minValue === None)   
  }
  
  test("3. NumericField[Double] with min and max value") {
    val f = new NumericField[Double]("requiredDoubleMinAndMax") {
      override val minValue = Some(-3.5)
      override val maxValue = Some(2.1)
    }
    assert(f.clean("") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean(Nil) === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("-4") === Left(ValidationError("This value must be at least -3.5.")))
    assert(f.clean("-3") === Right(-3.0))
    assert(f.clean("0") === Right(0.0))
    assert(f.clean("a") === Left(ValidationError("This value must be a number.")))
    assert(f.clean("2.11") === Left(ValidationError("This value must be at most 2.1.")))
    assert(f.clean("-1.5 ") === Right(-1.5))
    assert(f.clean(" 2.1") === Right(2.1))
    assert(f.clean(" -3.5 ") === Right(-3.5))
    assert(f.clean("    ") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("4a") === Left(ValidationError("This value must be a number.")))
    assert(f.maxValue === Some(2.1))
    assert(f.minValue === Some(-3.5))   
  }

  test("4. optional NumericField[Double] with min and max value") {
    val f = new NumericFieldOptional[Double]("optionalDoubleMinAndMax") {
      override val minValue = Some(-3.5)
      override val maxValue = Some(2.1)
    }
    assert(f.clean("") === Right(None))
    assert(f.clean(Nil) === Right(None))
    assert(f.clean("-4") === Left(ValidationError("This value must be at least -3.5.")))
    assert(f.clean("-3") === Right(Some(-3.0)))
    assert(f.clean("0") === Right(Some(0.0)))
    assert(f.clean("a") === Left(ValidationError("This value must be a number.")))
    assert(f.clean("2.11") === Left(ValidationError("This value must be at most 2.1.")))
    assert(f.clean("-1.5 ") === Right(Some(-1.5)))
    assert(f.clean(" 2.1") === Right(Some(2.1)))
    assert(f.clean(" -3.5 ") === Right(Some(-3.5)))
    assert(f.clean("    ") === Right(None))
    assert(f.clean("4a") === Left(ValidationError("This value must be a number.")))
    assert(f.maxValue === Some(2.1))
    assert(f.minValue === Some(-3.5))   
  }
  
  // EmailField *********************************************************************
  
  test("1. emailfield") {
    // TODO: are the commented-out tests right or wrong? We're using javax.mail
    //       to validate, so I'm kinda hoping it's right
    val f = new EmailField("default")
    assert(f.clean("") === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean(Nil) === Left(ValidationError("Expected a single value; got none or many.")))
    assert(f.clean("person@example.com") === Right("person@example.com"))
    assert(f.clean("foo") === Left(ValidationError("Enter a valid email address.")))
    assert(f.clean("foo@") === Left(ValidationError("Enter a valid email address.")))
    assert(f.clean("foo@bar") === Right("foo@bar")) //Left(ValidationError(List("Enter a valid email address."))))
    // TODO: these are invalid, but javax.mail doesn't check whether hyphens are only in the middle
    //assert(f.clean("example@invalid-.com") === Left(ValidationError("Enter a valid email address.")))
    //assert(f.clean("example@-invalid.com") === Left(ValidationError("Enter a valid email address.")))
    //assert(f.clean("example@inv-.alid-.com") === Left(ValidationError("Enter a valid email address.")))
    //assert(f.clean("example@inv-.-alid.com") === Left(ValidationError("Enter a valid email address.")))
    assert(f.clean("example@valid-----hyphens.com") === Right("example@valid-----hyphens.com"))
    assert(f.clean("example@valid-with-hyphens.com") === Right("example@valid-with-hyphens.com"))
    assert(f.clean("example@.com") === Left(ValidationError("Enter a valid email address.")))
    // TODO: this email is valid, but javax.mail doesn't allow Unicode characters
    //assert(f.clean("local@domain.with.idn.xyz\u00e4\u00f6\u00fc\u00dfabc.part.com") === Right("local@domain.with.idn.xyz\u00e4\u00f6\u00fc\u00dfabc.part.com"))
    // TODO: this email is invalid, but javax.mail doesn't check for a valid top-level domain
    //assert(f.clean("viewx3dtextx26qx3d@yahoo.comx26latlngx3d15854521645943074058") === Left(ValidationError("Enter a valid email address.")))
  }

  
  test("2. emailfield") {
    val f = new EmailFieldOptional("optional")
    assert(f.clean("") === Right(None))
    assert(f.clean(Nil) === Right(None))
    assert(f.clean("person@example.com") === Right(Some("person@example.com")))
    assert(f.clean("      example@example.com  \t   \t ") === Right(Some("example@example.com")))
    assert(f.clean("foo") === Left(ValidationError("Enter a valid email address.")))
    assert(f.clean("foo@") === Left(ValidationError("Enter a valid email address.")))
    assert(f.clean("foo@bar") === Right(Some("foo@bar"))) // Left(ValidationError(List("Enter a valid email adress."))))
  }
  
  test("3. emailfield") {
    val f = new EmailField("minAndMax.WHY?") { 
      override val minLength = Some(10)
      override val maxLength = Some(15)
    }
    assert(f.clean("a@foo.com") === Left(ValidationError("This value must have at least 10 characters. (It has 9.)")))
    assert(f.clean("alf@foo.com") === Right("alf@foo.com"))  
    assert(f.clean("alf123456788@foo.com") === Left(ValidationError("This value must have no more than 15 characters. (It has 20.)")))
  }
  
  //BooleanField******************************************************************
  /*
  test("1. booleanfield") {
    val f = new BooleanField("default")
    assert(f.clean("") === Left(ValidationError(List("This field is required"))))
    assert(f.clean(Nil) === Left(ValidationError(List("This field is requried"))))
    assert(f.clean(true) === Right(true))
    assert(f.clean(false) === Left(ValidationError(List("This field is requried"))))
    assert(f.clean("1") === Right(true))
    assert(f.clean("0") === Left(ValidationError(List("This field is requried"))))
    assert(f.clean("I rock") === Right(true))
    assert(f.clean("True") === Right(true))
    assert(f.clean("False") === Left(ValidationError(List("This field is requried"))))
  }
  
  test("2. booleanfield") {
    val f = new BooleanFieldOptional("optional")
    assert(f.clean("") === Right(Some(false)))
    assert(f.clean(Nil) === Right(Some(false)))
    assert(f.clean(true) === Right(Some(true)))
    assert(f.clean(false) === Right(Some(false)))
    assert(f.clean("1") === Right(Some(true)))
    assert(f.clean("0") === Right(Some(false)))
    assert(f.clean("Andrew Hamm rocks") === Right(Some(true)))
    assert(f.clean("False") === Right(Some(false)))
    assert(f.clean("false") === Right(Some(false)))
    assert(f.clean("FaLsE") === Right(Some(false)))
  }*/
  
  test("1. ChoiceField") {
    val f = new ChoiceField[Int]("grade", List("Freshman" -> 9, "Sophomore" -> 10, "Junior" -> 11, "Senior" -> 12))
    assert(f.clean("0") === Right(9))
    assert(f.clean("foo") === Left(ValidationError("Illegal value submitted.")))
    assert(f.clean("-1") === Left(ValidationError("This field is required. Please choose a value.")))
  }
  
}