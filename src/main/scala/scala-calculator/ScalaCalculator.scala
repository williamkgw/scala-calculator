package scala_calculator

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import com.raquo.laminar.api.L.{*, given}

import org.scalajs.dom

@main
def ScalaCalculator(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )

// import javascriptLogo from "/javascript.svg"
@js.native @JSImport("/javascript.svg", JSImport.Default)
val javascriptLogo: String = js.native

object Main:
  def appElement(): Element =
    div(
      a(href := "https://vitejs.dev", target := "_blank",
        img(src:= "/vite.svg", className := "logo", alt :="Vite logo")
      ),
      a(href := "https://developer.mozilla.org/en-US/docs/Web/JavaScript", target := "_blank"
        img(src := javascriptLogo, className :="logo vanilla", alt := "JavaScript logo")
      ),
      h1("Scala Calculator"),
      div(className := "card",
        button(id := "counter", typeName := "button")
      ),
      p(className := "read-the-docs", "Click on the Vite logo to learn more")
    )
  end appElement
end Main

def counterButton(): Element =
  val counter = Var(0)
  button(
    tpe := "button",
    "count is ",
    child.text <-- counter,
    onClick --> { event => counter.update(c => c + 1) },
  )
end counterButton

