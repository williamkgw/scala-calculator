package scala_calculator

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import com.raquo.laminar.api.L.{*, given}

import org.scalajs.dom

object UICalculator {
  def render(buttons: Seq[Seq[String]]): HtmlElement =
    table(
      UIDisplay.render(),
      UIButtons.render(buttons)
    )
}

object UIDisplay {
  def render(): HtmlElement =
    tr(
      td(
        colSpan := 3,
        input(value := "", readOnly := true),
      )
    )
}

object UIButtons {
  def render(buttons: Seq[Seq[String]]): Seq[HtmlElement] =
    buttons.map { row =>
      tr(
        row.map { label =>
          td(
            button(label)
          )
        }
      )
    }
}

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

