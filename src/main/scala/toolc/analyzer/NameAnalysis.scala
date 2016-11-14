package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def collectSymbols(prog: Program): GlobalScope = {

      val global = new GlobalScope

      val mcSym = new MainSymbol(prog.main.id.value)
      global.mainClass = mcSym
      prog.main.setSymbol(mcSym)
      prog.main.id.setSymbol(mcSym)
       
      // TODO: Create empty symbols for all classes, checking that their names are unique
      for (c <- prog.classes) {
        if (global.classes.contains(c.id.value)) {
          error("At least two classes have the same name.")
        } else {
          // throwing error:
          if (c.id.value == "Object") {
            error("No classes can't be named 'Object'.")
          }
          val cl = new ClassSymbol(c.id.value)
          global.classes.+((c.id.value, cl)) // pas sûr sûr!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        }
      }

      // Set parent Symbols
      for {
        cls <- prog.classes
        clSym = global.classes(cls.id.value)
        par <- cls.parent
      } yield {
        global.lookupClass(par.value) match {
          case None =>
            error(s"Class ${clSym.name} extends class ${par.value} which is not defined.", par)
          case Some(parSym) =>
            clSym.parent = Some(parSym)
            par.setSymbol(parSym)
        }
      }

      // Check there are no cycles in the inheritance graph
      prog.classes foreach { cls =>
        val clsSym = cls.getSymbol

        def mkChain(curr: ClassSymbol): List[ClassSymbol] = {
          curr.parent match {
            case None => List(curr)
            case Some(`clsSym`) => List(curr, clsSym)
            case Some(p) => curr :: mkChain(p)
          }
        }

        val chain = mkChain(clsSym)

        if (chain.size > 1 && chain.head == chain.last) {
          fatal("Cyclic inheritance: " + chain.map(_.name).mkString(" -> "))
        }

      }

      // We now know that every class is unique and the inheritance graph is
      // correct. We proceed to check the contents of these classes.
      prog.classes.foreach(collectInClass)

      def collectInClass(c: ClassDecl): Unit = {
        // TODO: Traverse a class to collect symbols and emit errors
        //       in case a correctness rule of Tool is violated
        // Note: It is important that you analyze parent classes first (Why?)
        
        //case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
        // je dois ajouter aux ClassSymbol de global.classes
        
        // first the parents
        val toOverride: Option[ClassSymbol] = {
          c.parent match {
            case Some(p) => global.classes.get(p.value) // toOverride is the ClassSymbol of the parent if it exists
            case None => None
        }}
       
        // first I collect the methods of the class:
        for (m <- c.methods) {
          val mSym = new MethodSymbol(m.id.value, global.classes(m.id.value)).setPos(m)
          
          for (p <- m.vars) {
            mSym.members += (p.id.value -> new VariableSymbol(p.id.value).setPos(p))
            // verify that two members have not the same name
            if (m.vars.filter { x => (x.id.value==p.id.value)}.size != 1) {
              error("Two members of one method can't have the same name.")
            }
          }
          
          for (a <- m.args) {
            val aSym = new VariableSymbol(a.id.value).setPos(a)
            mSym.params += (a.id.value -> aSym)
            mSym.argList = mSym.argList :+ aSym
            // verify that two arguments have note the same name
            if (m.args.filter { x => (x.id.value==a.id.value) }.size != 1) {
              error("Two arguments of one method can't have the same name.")
            }
          }
          // verify 
          if (m.args.map{ x => x.id.value }.intersect(m.vars.map{ x => x.id.value }) != 0) {
            error("In a method, two parameters/local variables can't have the same name.")
          }
          
          toOverride match { // control if the method override another one
            case Some(clSym) => {
              clSym.methods.get(m.id.value) match {
                case Some(mtdSym) => {
                  if (mtdSym.argList.size != mSym.argList.size) { // the two methods must have the same number of arguments
                    error("A method can't override another with a different number of parameters.")
                  }
                  mSym.overridden = clSym.methods.get(m.id.value)
                }
                case None => 
              }
            }
            case None =>
          }
          
          // Add the method to the ClassSymbol corresponding: 
          global.classes(c.id.value).methods += (m.id.value -> mSym)
        }
        
        // second the variables: 
        for (v <- c.vars) {
          val vSym = new VariableSymbol(v.id.value).setPos(v)
          // Verify that two variables have not the same name
          if (c.vars.filter { x => (x.id.value==v.id.value) }.size != 1) {
            error("Two variables of one class can't have the same name.")
          }
          // Verify that a variable has not the same name has a variable inside an inherited class:
          toOverride match {
            case Some(clSym) => {
              if (clSym.members.contains(v.id.value)) {
                error("A variable can't have the same name as a variable inside an inherited class.")
              }
            }
            case None => 
          }
          // Add the variable to the ClassSymbol corresponding: 
          global.classes(c.id.value).members += (v.id.value -> vSym)
        }
      }
     
      global
    }

    def setPSymbols(prog: Program, gs: GlobalScope): Unit = {
      // TODO: Traverse within each definition of the program
      //       and attach symbols to Identifiers and "this"
      prog.classes.map{ x => setCSymbols(x, gs)} 
      
    }

    def setCSymbols(klass: ClassDecl, gs: GlobalScope): Unit = {
      val classSym = gs.lookupClass(klass.id.value).get
      for (varDecl <- klass.vars) {
        setTypeSymbol(varDecl.tpe, gs)
      }

      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol): Unit = {
      val methodSym = cs.lookupMethod(meth.id.value).get
      for (varDecl <- meth.vars) {
        setTypeSymbol(varDecl.tpe, gs)
      }
//      class MethodDecl(id: Identifier,
//                        args: List[Formal],
//                        retType: TypeTree,
//                        vars: List[VarDecl],
//                        stats: List[StatTree],
//                        retExpr: ExprTree)
      for (args <- meth.args) {
        setTypeSymbol(args.tpe, gs)
      }
      setESymbols(meth.retExpr)(gs, Some(methodSym))
      setTypeSymbol(meth.retType, gs)
      meth.stats.foreach(setSSymbols(_)(gs, Some(methodSym)))
      
    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {
      
      stat match {
        case Block(stats) => stats.foreach(setSSymbols(_))
        case If(expr, thn, els) => {
          setESymbols(expr)
          setSSymbols(thn)
          els.getOrElse(Nil) match {
            case st: StatTree => setSSymbols(st)
            case Nil =>
          }
        }
        case While(expr, stat) => {
          setESymbols(expr)
          setSSymbols(stat)
        }
        case Println(expr) => setESymbols(expr)
        case Assign(id, expr) => {
          setISymbol(id)
          setESymbols(expr)
        }
        case ArrayAssign(id, index, expr) => {
          setISymbol(id)
          setESymbols(index)
          setESymbols(expr)
        }
        case DoExpr(e) => {
          setESymbols(e)
        }
      }
    }

    def setISymbol(id: Identifier)(implicit ms: Option[MethodSymbol]) = {
      // in this context, it will always be an expression (variable)
      ms.flatMap(_.lookupVar(id.value)) match {
        case None =>
          error("Undeclared identifier: " + id.value + ".", id)
        case Some(sym) =>
          id.setSymbol(sym)
      }
    }

    def setESymbols(expr: ExprTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {
      
      expr match {
        case And(lhs, rhs) => {
          setESymbols(lhs)
          setESymbols(rhs)
        }
        case Or(lhs, rhs) => {
          setESymbols(lhs)
          setESymbols(rhs)
        }
        case Plus(lhs, rhs) => {
          setESymbols(lhs)
          setESymbols(rhs) 
        }
        case Minus(lhs, rhs) => {
          setESymbols(lhs)
          setESymbols(rhs)
        }
        case Times(lhs, rhs) => {
          setESymbols(lhs)
          setESymbols(rhs)
        }
        case Div(lhs, rhs) => {
          setESymbols(lhs) 
          setESymbols(rhs) 
        }
        case LessThan(lhs, rhs) => {
          setESymbols(lhs)
          setESymbols(rhs)
        }
        case Equals(lhs, rhs) => {
          setESymbols(lhs)
          setESymbols(rhs)
        }
        case ArrayRead(arr, index) => {
          setESymbols(arr)
          setESymbols(index)
        }
        case ArrayLength(arr) => setESymbols(arr)
        case MethodCall(obj, meth, args) => {
          setESymbols(obj)
          setISymbol(meth)
          args.foreach(setESymbols(_))
        }
        case Variable(id) => setISymbol(id)
        case t: This => { //  case class This() extends ExprTree with Symbolic[ClassSymbol]
          // TODO faire attention pas de this dans Main 
          t.setSymbol(ms.get.classSymbol)
        }
        case NewIntArray(size) => setESymbols(size)
        case New(tpe) => setISymbol(tpe)
        case Not(expr) => setESymbols(expr)
        case _ => 
      }
    }

    def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = {
      // Types
//  case class ClassType(id: Identifier) extends TypeTree
      tpe match {
        case ClassType(id) => setISymbol(id)(None) // TODO normalement rejette une erreur??
        case _ => 
      }
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    prog
  }
}
