package sandbox

import scala.quoted._

object MacroUtils:

  object PrintMacPass:
    inline def apply[T](inline any: T): T = ${ printMacImpl('any) }

    def printMacImpl[T: Type](any: Expr[T])(using ctx: Quotes): Expr[T] =
      import ctx.reflect._
      println(Printer.TreeShortCode.show(any.asTerm))
      any
