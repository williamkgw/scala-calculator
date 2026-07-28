package scala_calculator

import com.raquo.laminar.api.L.{*, given}

trait Rendable {
  def render(): HtmlElement | Seq[HtmlElement]
}


class UIButton(buttonText: String, eventButton: EventBus[String]) extends Rendable {
  override def render(): HtmlElement =
    button(
      buttonText,
      onClick.map { _ =>
        buttonText
      } --> eventButton
    )
}


class UIDisplay(eventButton: EventBus[String]) extends Rendable {
  import ExpressionOperations.*

  def updateDisplay(display: String, button: String): String =
    button match {
      case "DEL" =>
        display.substring(0, display.size - 1)

      case "=" =>
        val tokens = Tokenizer.tokenize(display)
        val (tree, rest) = AbstractSyntaxTreeParser.parseExpression(tokens)
        Evaluator.evaluate(tree).toString

      case "ON" =>
        display

      case value =>
        display + value
    }

  override def render(): HtmlElement =
    val displayText = eventButton.events.scanLeft("")(
        (display, button) => updateDisplay(display, button)
    )

    tr(
      td(
        colSpan := 4,
        input(value <-- displayText, readOnly := true),
      )
    )
}


class UIButtons(buttons: Seq[Seq[String]], eventButton: EventBus[String]) extends Rendable {
  override def render(): Seq[HtmlElement] =
    buttons.map { row =>
      tr(
        row.map { label =>
          td(
            UIButton(label, eventButton).render()
          )
        }
      )
    }
}


class UICalculator(buttons: Seq[Seq[String]]) extends Rendable {
  override def render(): HtmlElement =
    val eventButton: EventBus[String] = new EventBus[String]

    table(
      UIDisplay(eventButton).render(),
      UIButtons(buttons, eventButton).render()
    )
}

