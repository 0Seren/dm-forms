package templates

import play.api.mvc.Request
import scalatags._
import _root_.forms.Binding
import _root_.forms.Call
import Call._

package for_forms {
  object Tester {
    def apply(form: Binding)(implicit req: Request[_]) = {
      html(
        head(
          title("Form Tester"),
          link.rel("stylesheet").href(controllers.routes.WebJarAssets.at(controllers.WebJarAssets.locate("jquery-ui.css"))),
          link.rel("stylesheet").href(controllers.routes.WebJarAssets.at(controllers.WebJarAssets.locate("datepicker.css"))),
          link.rel("stylesheet").href(controllers.routes.WebJarAssets.at(controllers.WebJarAssets.locate("bootstrap-timepicker.min.css"))),
          link.rel("stylesheet").href(controllers.routes.WebJarAssets.at(controllers.WebJarAssets.locate("bootstrap.css"))),
          <!-- HTML5 shim, for IE6-8 support of HTML5 elements. -->,
          <!--[if lt IE 9]>
            <script src="http://html5shim.googlecode.com/svn/trunk/html5.js"></script>
          <![endif]-->,
          script.src(controllers.routes.WebJarAssets.at(controllers.WebJarAssets.locate("jquery.js"))),
          script.src(controllers.routes.WebJarAssets.at(controllers.WebJarAssets.locate("jquery-ui.js"))),
          script.src(controllers.routes.WebJarAssets.at(controllers.WebJarAssets.locate("bootstrap.js"))),
          script.src(controllers.routes.Assets.at("javascripts/maskedinput.js")),
          script.src(controllers.routes.WebJarAssets.at(controllers.WebJarAssets.locate("bootstrap-datepicker.js"))),
          script.src(controllers.routes.WebJarAssets.at(controllers.WebJarAssets.locate("bootstrap-timepicker.min.js")))),
	    body(
          h1("Form Tester"),
          p("All of these fields are optional and should be used to test how the different types of fields work."),
          p(form.render(legend=Some("This Is A Legend"))))).toXML.toString
    }
  }
  
  object Results {
    def apply(list: List[(String, String)])(implicit req: Request[_]) = {
      html(
        head(
          title("Results")),
        body(list.map(kv => p(kv._1 + "--->" + kv._2)))).toXML.toString
    }
  }
}
