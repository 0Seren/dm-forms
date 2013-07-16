package forms

import scala.xml.NodeSeq
import scala.xml.Utility.trim

import widgets._
import Widget._

import org.scalatest.FunSuite

class TestWidgets extends FunSuite {
  test("TextInput widget") {
    val ti1 = new TextInput(true, attrs = Map("class"->"foo"))
    assert(ti1.render("entry", Nil) === <input type="text" name="entry" class="foo" required="required" />)
    assert(ti1.render("entry", List("abc")) === 
      <input type="text" name="entry" value="abc" class="foo" required="required" />)
  }
  
  test("PasswordInput widget") {
    val pi1 = new PasswordInput(true)
    val pi2 = new PasswordInput(true, renderValue = true)
    assert(pi1.render("pw", List("pass")) === <input type="password" name="pw" required="required"/>)
    assert(pi2.render("pw", List("pass")) === <input type="password" name="pw" value="pass" required="required" />)
  }
  
  test("HiddenInput widget") {
    val hi1 = new HiddenInput(true, attrs = Map("class"->"foo"))
    assert(hi1.render("key1", List("value1")) === 
        <input type="hidden" name="key1" value="value1" class="foo" required="required" />)
  }
  
  test("Textarea widget") {
    val ta = new Textarea(false)
    assert(ta.render("words", List("abc")) ===
      <textarea name="words" cols="80" rows="5">abc</textarea>)
    assert(ta.render("moreWords", Nil, Map("rows"->"10")) ===
      <textarea name="moreWords" cols="80" rows="10"></textarea>)
    assert(ta.render("wordsAlso", List("xyz"), Map("cols"->"40")) ===
      <textarea name="wordsAlso" cols="40" rows="5">xyz</textarea>)
  }
  
  test("CheckboxInput widget") {
    val cb = new CheckboxInput(false, List("true"))
    assert(cb.render("box", List("true")) ===
      <fieldset name="box">
        <input type="checkbox" name="box" checked="checked" />true<br/>
      </fieldset>)
  }
  
  test("SelectInput widget") {
    val s = new SelectInput(true, List("John", "Paul", "George", "Ringo"))
    assert(trim(s.render("beatle", List("0"))) === trim(
      <select name="beatle">
        <option value="0" selected="selected">John</option>
    	<option value="1">Paul</option>
    	<option value="2">George</option>
    	<option value="3">Ringo</option>
      </select>))
    assert(trim(s.render("beatle", Nil)) === trim(
      <select name="beatle">
        <option value="-1"> </option>
        <option value="0">John</option>
        <option value="1">Paul</option>
    	<option value="2">George</option>
    	<option value="3">Ringo</option>
      </select>))
    assert(trim(s.render("beatle", List("John"))) === trim(
      <select name="beatle">
        <option value="-1"> </option>
        <option value="0">John</option>
        <option value="1">Paul</option>
    	<option value="2">George</option>
    	<option value="3">Ringo</option>
      </select>))  
    // unless allowsMultiple is true, only the first element in the list is selected
    assert(trim(s.render("beatle", List("1", "2"))) === trim(
      <select name="beatle">
        <option value="0">John</option>
        <option value="1" selected="selected">Paul</option>
    	<option value="2">George</option>
    	<option value="3">Ringo</option>
      </select>))  
  }
}