package scala_calculator

import com.raquo.laminar.api.L.{*, given}

trait Rendable {
  def render(): HtmlElement | Seq[HtmlElement]
}


class UIButton(buttonText: String) extends Rendable {
  override def render(): HtmlElement =
    button(buttonText)
}


class UIDisplay extends Rendable {
  override def render(): HtmlElement =
    tr(
      td(
        colSpan := 4,
        input(value := "", readOnly := true),
      )
    )
}


class UIButtons(buttons: Seq[Seq[String]]) extends Rendable {
  override def render(): Seq[HtmlElement] =
    buttons.map { row =>
      tr(
        row.map { label =>
          td(
            UIButton(label).render()
          )
        }
      )
    }
}


class UICalculator(buttons: Seq[Seq[String]]) extends Rendable {
  override def render(): HtmlElement =
    table(
      UIDisplay().render(),
      UIButtons(buttons).render()
    )
}

