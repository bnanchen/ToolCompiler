package toolc
package analyzer

import ast.Trees._

import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcClass(klass: ClassDecl): Unit = klass.methods.foreach(tcMethod)

    /** Type checks statements and return expression of the method */
    def tcMethod(meth: MethodDecl): Unit = {
      // TODO Done.
      meth.stats.foreach { x => tcStat(x) }
      tcExpr(meth.retExpr, meth.retType.getType)
      if (!(meth.retExpr.getType.isSubTypeOf(meth.retType.getType))) {
        sys.error("The returned expression must be of a subtype of the declared return type.")
      }
    }

    /** Checks that the expression is a subtype of the ones in expectedTps.
      * If it's not, prints an error message and returns the error type.
      * Also adds missing symbols to methods in MethodCalls
      */
    def tcExpr(expr: ExprTree, expectedTps: Type*): Unit = {
      expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        case Not(expr) =>
          tcExpr(expr, TBoolean)
        case p: Plus => 
          // TODO JUSTE??
          p.getType // useful??
          tcExpr(p.lhs, TInt, TString)
          tcExpr(p.rhs, TInt, TString)
        case Minus(lhs, rhs) => 
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        case Equals(lhs, rhs) => {
          (lhs.getType, rhs.getType) match {
            case (TInt, TInt) => 
              tcExpr(lhs, TInt)
              tcExpr(rhs, TInt)
            case (TString, TString) =>
              tcExpr(lhs, TString)
              tcExpr(rhs, TString)
            case (TBoolean, TBoolean) =>
              tcExpr(lhs, TBoolean)
              tcExpr(rhs, TBoolean)
            case (TIntArray, TIntArray) =>
              tcExpr(lhs, TIntArray)
              tcExpr(rhs, TIntArray)
            case (TClass(x), TClass(y)) => // TODO JUSTE?? la manière de matcher le TClass?!?
              tcExpr(lhs, TObject)
              tcExpr(rhs, TObject)
            case _ => 
              sys.error("The right and left handsides either belong to class types or are of the same type (Int, String, Boolean, Int[]).")
          }
        }
        case ArrayRead(arr, index) =>
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray)
        case NewIntArray(size) =>
          tcExpr(size, TInt)
        case m: MethodCall => {
          // TODO JUSTE??
          m.getType // usefule car peut-être appelé dessous...voir Cédric!
          tcExpr(m.obj)
          m.args.foreach { x => tcExpr(x) }
        }
        case _ => 
      }
      
      // s'occupe de regarder si expr is a subtype of one of the types in expectedTps
      if (!expectedTps.toList.exists(expr.getType.isSubTypeOf)) {
        error("Type error: Expected: " + expectedTps.mkString(" or ") + s", found: ${expr.getType}", expr)
      }

    }
 
    /** Invokes tcExpr as needed in the expressions of stat */
    def tcStat(stat: StatTree): Unit = {
      // TODO
      stat match {
        case Block(stats) =>
          stats.foreach { x => tcStat(x) }
        case If(expr, thn, els) =>
          tcExpr(expr, TBoolean)
          tcStat(thn)
          els match {
            case Some(s) => tcStat(s)
            case None => 
          }
        case While(expr, stat) => 
          tcExpr(expr, TBoolean)
          tcStat(stat)
        case Println(expr) =>
          tcExpr(expr, TInt, TString, TBoolean)
        case Assign(id, expr) => {
          if (!(expr.getType.isSubTypeOf(id.getType))) {
            sys.error("Assignment of an expression of type T can only be done to a variable of type S such that T <: S.")
          }
          tcExpr(expr, TBoolean, TString, TInt, TObject) // TODO comment tu fais quand tu veux envoyer TClass dans tcExpr??
        }
        case ArrayAssign(id, index, expr) => {
         if (id.getType != TIntArray) {
           sys.error("Assignment to array elements can only be done through an array variable.")
         }
         tcExpr(index, TInt)
         tcExpr(expr, TInt)
        }
        case DoExpr(e) =>
          tcExpr(e, TInt, TBoolean, TString, TIntArray, TObject)
      }
    }
 
    prog.main.stats.foreach(tcStat)
    prog.classes.foreach(tcClass)

    prog
  }
}
