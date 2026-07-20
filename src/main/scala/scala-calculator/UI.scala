package scala_calculator

import com.raquo.laminar.api.L.{*, given}

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

