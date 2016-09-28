package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    val ectx = new MainContext
    prog.main.stats.foreach(evalStatement(_)(ectx))
  }

  def evalStatement(stmt: StatTree)(implicit ectx: EvaluationContext): Unit = stmt match {
    case Block(stats) => {
      stats.foreach { s => evalStatement(s) } // juste?
    }
    case If(expr, thn, els) => {
      if(evalExpr(expr).asBool) {
        evalStatement(thn)
      } else {
        els match {
          case None => None
          case Some(e) => evalStatement(e)
        }
      }
    }
    case While(expr, stat) => {
      while(evalExpr(expr).asBool) {
        evalStatement(stat)
      }
    }
    case Println(expr) => {
      val e = evalExpr(expr)
      e match {
        case IntValue(v) => println(v)
        case BoolValue(v) => println(v)
        case StringValue(v) => println(v)
        case _ => fatal("Not accepted by println")
      }
    }
    case Assign(id, expr) => {
      ectx.setVariable(id.value, evalExpr(expr))
    }
    case ArrayAssign(id, index, expr) => {
      ectx.getVariable(id.value).asArray.setIndex(evalExpr(index).asInt, evalExpr(expr).asInt)
    }
    case DoExpr(expr) => {
      val exp = evalExpr(expr)
    }
  }

  def evalExpr(e: ExprTree)(implicit ectx: EvaluationContext): Value = e match {
    case IntLit(value) => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True() => BoolValue(true)
    case False() => BoolValue(false)
    case And(lhs, rhs) => BoolValue(evalExpr(lhs).asBool && evalExpr(rhs).asBool)
    case Or(lhs, rhs)  => BoolValue(evalExpr(lhs).asBool || evalExpr(rhs).asBool)
    case Plus(lhs, rhs) => {
      val l = evalExpr(lhs)
      val r = evalExpr(rhs)
      (l,r) match {
        case (StringValue(v1), StringValue(v2)) => StringValue(v1 + v2)
        case (StringValue(v1), IntValue(v2)) => StringValue(v1 + v2.toString)
        case (IntValue(v1), StringValue(v2)) => StringValue(v1.toString + v2)
        case (IntValue(v1), IntValue(v2)) => IntValue(v1 + v2)
        case _ => fatal("Not accepted addition")
      }
    }
    case Minus(lhs, rhs) => {
      val l = evalExpr(lhs)
      val r = evalExpr(rhs)
      (l, r) match {
        case (IntValue(v1), IntValue(v2)) => IntValue(v1 - v2)
        case _ => fatal("Not accepted soustraction")
      }
    }
    case Times(lhs, rhs) => {
      val l = evalExpr(lhs)
      val r = evalExpr(rhs)
      (l, r) match {
        case (IntValue(v1), IntValue(v2)) => IntValue(v1 * v2)
        case _ => fatal("Not accepted multiplication")
      }
    }
    case Div(lhs, rhs) => {
      val l = evalExpr(lhs)
      val r = evalExpr(rhs)
      (l, r) match {
        case (IntValue(v1), IntValue(v2)) => IntValue(v1 / v2)
        case _ => fatal("Not accepted division")
      }
    }
    case LessThan(lhs, rhs) => {
      val l = evalExpr(lhs)
      val r = evalExpr(rhs)
      (l, r) match {
        case (IntValue(v1), IntValue(v2)) => BoolValue(v1 < v2)
        case _ => fatal("Not accepted for less than")
      }
    }
    case Not(expr) => {
      BoolValue(!evalExpr(expr).asBool)
    }
    case Equals(lhs, rhs) => {
      val l = evalExpr(lhs)
      val r = evalExpr(rhs)
      (l, r) match {
        case (IntValue(v1), IntValue(v2)) => BoolValue(v1 == v2)
        case (BoolValue(v1), BoolValue(v2)) => BoolValue(v1 == v2)
        case (_: ArrayValue, _: ArrayValue) => BoolValue(l eq r) // eq utilisé par comparé 2 tableaux
        case (StringValue(v1), StringValue(v2)) => BoolValue(v1 eq v2)
        case (ObjectValue(v1), ObjectValue(v2)) => BoolValue(v1 eq v2)
        case _ => fatal("Not accepted for equals: type error")
      }
    }
    case ArrayRead(arr, index) => {
      val i = evalExpr(index).asInt
      IntValue(evalExpr(arr).asArray.getIndex(i))
    }
    case ArrayLength(arr) => {
      evalExpr(arr).asArray match {
        case ArrayValue(a) => IntValue(a.length)
        case _ => fatal("Not accepted for array length")
      }
    }
       // creer context, evaluer la méthode: 1. évaluer arguments tu dois les
       // assigner MethodDecl set variable avec MethodDecl. 2. évaluer la méthode: 
    case MethodCall(obj, meth, args) => {
       val method = findMethod(evalExpr(obj).asObject.cd, meth.value) // obtenu la méthode, type: MethodDecl
       val ob = evalExpr(obj).asObject
       val ctx = new MethodContext(ob) // création d'un context
       var i = 0
       while (i < args.length) {
         val e = evalExpr(args(i))
         ctx.declareVariable(method.args(i).id.value)
         ctx.setVariable(method.args(i).id.value, e)  
         i += 1
       }
       i = 0
       while (i < method.vars.length) {
         ctx.declareVariable(method.vars(i).id.value)
         i += 1
       }
       //method.stats foreach(v => evalStatement(v)(ctx))
       i = 0
       while (i < method.stats.length) {
         evalStatement(method.stats(i))(ctx)
         i += 1
       }
       evalExpr(method.retExpr)(ctx) // ?? 20th century women
    }
    case Variable(Identifier(name)) => {
      ectx.getVariable(name)
    }
    case New(tpe) => {
      val a = findClass(tpe.value)
      val ob = ObjectValue(a)
      var i = 0
      while(i < a.vars.length) {
        ob.declareField((a.vars(i).id.value))
        i += 1 
      }
      ob
      
    }
    case This() => {
      ectx match {
        case ectx: MethodContext => ectx.obj
        case ectx: MainContext => fatal("dans le MainContext")
      }
    }
    case NewIntArray(size) => {
      ArrayValue(new Array[Int](evalExpr(size).asInt))
    }
  }

  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '"+name+"'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
  }

  class MainContext extends EvaluationContext {
    private def unavailable = fatal("The main object contains no variables and/or fields")
    def getVariable(name: String): Value          = unavailable
    def setVariable(name: String, v: Value): Unit = unavailable
    def declareVariable(name: String): Unit       = unavailable
  }

  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.value), name))
    ).getOrElse(fatal("Unknown method "+cd.id+"."+name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  sealed abstract class Value {
    private def expected(tp: String) = fatal(s"Unexpected value: found $this, expected $tp")

    def asInt: Int            = expected("Int")
    def asString: String      = expected("String")
    def asBool: Boolean       = expected("Boolean")
    def asObject: ObjectValue = expected("Object")
    def asArray: ArrayValue   = expected("Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal(s"Unknown field '$name'")
      }
    }

    def getField(name: String) = {
      fields.get(name) match {
        case Some(Some(v)) => v
        case Some(None) => fatal(s"Field '$name' has not been initialized")
        case None => fatal(s"Unknown field '$name'")
      }
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(entries: Array[Int]) extends Value {
    val length = entries.length

    private def checkBounds(index: Int) = {
      if (index < 0 || index >= length) {
        fatal(s"Index '$index' out of bounds (0 .. ${length-1})")
      }
    }

    def setIndex(i: Int, v: Int) {
      checkBounds(i)
      entries(i) = v
    }

    def getIndex(i: Int) = {
      checkBounds(i)
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(v: String) extends Value {
    override def asString = v
  }

  case class IntValue(v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(v: Boolean) extends Value {
    override def asBool = v
  }
}
