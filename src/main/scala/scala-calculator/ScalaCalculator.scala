package scala_calculator

import org.scalajs.dom
import com.raquo.laminar.api.L.{renderOnDomContentLoaded}

object App {
  def main(args: Array[String]): Unit =
    val buttons = Seq(
      Seq("(", "DEL", ")", "/"),
      Seq("1", "2", "3", "X"),
      Seq("4", "5", "6", "-"),
      Seq("7", "8", "9", "+"),
      Seq("ON", "0", ",", "="),
    )

    lazy val appContainer = dom.document.querySelector("#app")
    val appElement = UICalculator(buttons).render()
    renderOnDomContentLoaded(appContainer, appElement)
}

