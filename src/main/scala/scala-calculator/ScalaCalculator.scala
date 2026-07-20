package scala_calculator

import org.scalajs.dom
import com.raquo.laminar.api.L.{renderOnDomContentLoaded}

object App {
  def main(args: Array[String]): Unit =
    val buttons = Seq(
      Seq("1", "2", "3"),
      Seq("4", "5", "6"),
      Seq("7", "0", "9"),
    )

    lazy val appContainer = dom.document.querySelector("#app")
    val appElement = UICalculator.render(buttons)
    renderOnDomContentLoaded(appContainer, appElement)
}

