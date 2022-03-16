package sandbox.semantics

import scala.tasty.inspector.*
import scala.quoted.*
import dotty.tools.dotc.ast.Trees.PackageDef
import dotty.tools.dotc.core.Contexts.Context

object Examples:
  val userO: Option[String] = Some("Bela")
  val barO:  String         = "oh no, anyway"

class MyInspector extends Inspector:

  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*

    def collectStuff(tree: Tree)(using ctx: Context): Unit =
      val walker = new TreeAccumulator[Unit]:
        def foldTree(acc: Unit, tree: Tree)(owner: Symbol): Unit =
          tree match
            case ValDef(name, wat, rhs) =>
              println(s"VAL: ${name}, ?=${wat}, rhs=${rhs}")
            case other =>
              foldOverTree((), other)(owner)
      walker.foldTree((), tree)(tree.symbol)

    for tasty <- tastys do collectStuff(tasty.ast)(using dotty.tools.dotc.core.Contexts.NoContext.given_Context)

@main
def main(): Unit =
  val tastyFiles = List("target/scala-3.0.1/classes/sandbox/semantics/Examples.tasty")
  TastyInspector.inspectTastyFiles(tastyFiles)(MyInspector())
