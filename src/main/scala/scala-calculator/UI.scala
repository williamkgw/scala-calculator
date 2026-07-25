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

  def TreatDisplayText(displayText: Signal[String]) =
    val expression = "1 + 2 + 3 + 10 + 20 + 30"
    val tokens = Tokenizer.tokenize(expression)
    val (tree, rest) = AbstractSyntaxTreeParser.parseExpression(tokens)

    displayText.map {
      a => s"$tree / $tokens"
    }

  override def render(): HtmlElement =
    val displayText = TreatDisplayText(eventButton.events.scanLeft("")(_ + _))
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

