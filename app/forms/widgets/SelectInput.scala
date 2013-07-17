package forms.widgets

import scala.xml._

/**
 * A widget that creates a select input. 
 */
class SelectInput(
    required: Boolean,
    val options: List[String],
    attrs: MetaData = Null,
    val allowMultiple: Boolean = false) extends Widget(required, attrs) {

  /**
   * Renders the select input using xml.
   */
  def render(name: String, value: Seq[String], attrList: MetaData = Null) = {
    // if allowMultiple is false, only allow one value
    val limitedVal = if (allowMultiple) value else if (value.isEmpty) Nil else List(value.head)
    val multAttr = if (allowMultiple) new UnprefixedAttribute("multiple", Text("multiple"), Null) else Null
    <select name={ name }>
      { if (!realValue(limitedVal)) <option value="-1"> </option> else NodeSeq.Empty }
      { options.zipWithIndex.map { 
        case (disp, i) => {
          val selAttr = if (limitedVal.contains(i.toString)) new UnprefixedAttribute("selected", Text("selected"), Null) else Null
          <option value={ i.toString }>{ disp }</option> % selAttr
        }
      }
    }
    </select> % attrs % multAttr % attrList // <select> doesn't allow a required attribute
  }
  
  /**
   * Creates the real values to be returned.
   */
  private[this] def realValue(value: Seq[String]): Boolean = {
    def goodIndex(s: String): Boolean = {
      try {
        0 <= s.toInt && s.toInt < options.length
      } catch {
        case e: NumberFormatException => false
      }
    }
    !value.isEmpty && value.forall(goodIndex(_))
  }
}