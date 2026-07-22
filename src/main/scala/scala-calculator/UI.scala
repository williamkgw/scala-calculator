package scala_calculator

import com.raquo.laminar.api.L.{*, given}

// Calculator expression compiler design
//
//
// String
// |
// |
// Tokenizer
// |
// |
// List[Token]
// |
// |
// Parser
// |
// |
// Expression (AST)
// |
// |
// process()
// |
// |
// Int

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
    // example of Tokenizer
    val tokens = Tokenizer.tokenize(expression)

    // output of display text expression
    val expr =
      Operation(
        Operation(
          Value(1),
          Operation(Value(2), Value(3), Sum),
          Sum
        ),
        Operation(
          Operation(
            Value(10),
            Operation(Value(20), Value(30), Sum),
            Sum
          ),
          Value(0),
          Sum
        ),
        Sum
      )

    val exprResult = expr.process()

    displayText.map {
      a => s"$exprResult $tokens"
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

