//Katherine Mayo

import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike {
	def eval(e: Expr): Double = e match {
		case Add(e1: Expr, e2: Expr) => eval(e1) + eval(e2)
		case Div(e1: Expr, e2: Expr) => eval(e1) / eval(e2)
		case Exponent(e1: Expr, e2: Expr) => scala.math.pow(eval(e1), eval(e2))
		case Mul(e1: Expr, e2: Expr) => eval(e1) * eval(e2)
		case Num(n: Double) => n
		case Sub(e1: Expr, e2: Expr) => eval(e1) - eval(e2)
	}
}

object ArithParser extends ArithParserLike {
	//number: PackratParser [Double] is defined in ArithParserLike

	lazy val add: PackratParser[Expr] = (add~"+"~mul^^{case e1~"+"~e2 => Add(e1, e2)}) | (add~"-"~mul^^{case e1~"-"~e2 => Sub(e1, e2)}) | mul
	lazy val mul: PackratParser[Expr] = (exponent~"*"~mul^^{case e1~"*"~e2 => Mul(e1, e2)}) | (exponent~"/"~mul^^{case e1~"/"~e2 => Div(e1, e2)}) | exponent
	lazy val exponent: PackratParser[Expr] = (exponent~"^"~atom^^{case e1~"^"~e2 => Exponent(e1, e2)}) | atom
	lazy val expr: PackratParser[Expr] = add
	lazy val atom: PackratParser[Expr] =  (number^^{case x => Num(x)}) | ("("~expr~")"^^{case"("~e1~")" => e1})
}

object ArithPrinter extends ArithPrinterLike {
	def print(e: Expr): String = e match {
		case Add(e1: Expr, e2: Expr) => "(" + print(e1) + " + " + print(e2) + ")"
		case Div(e1: Expr, e2: Expr) => "(" + print(e1) + " / " + print(e2) + ")"
		case Exponent(e1: Expr, e2: Expr) => "(" + print(e1) + " ^ " + print(e2) + ")"
		case Mul(e1: Expr, e2: Expr) => "(" + print(e1) + " * " + print(e2) + ")"
		case Num(n: Double) => "(" + n + ")"
		case Sub(e1: Expr, e2: Expr) => "(" + print(e1) + " - " + print(e2) + ")"
	}
}