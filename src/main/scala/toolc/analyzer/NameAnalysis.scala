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
          val mSym = new MethodSymbol(m.id.value, global.classes(m.id.value))
          
          for (p <- m.vars) {
            mSym.members += (p.id.value -> new VariableSymbol(p.id.value))
            // verify that two members have not the same name
            if (m.vars.filter { x => (x.id.value==p.id.value)}.size != 1) {
              error("Two members of one method can't have the same name.")
            }
          }
          
          for (a <- m.args) {
            val aSym = new VariableSymbol(a.id.value)
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
          val vSym = new VariableSymbol(v.id.value)
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
      
    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = {
      ??? // TODO
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
      ??? // TODO
    }

    def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = {
      ??? // TODO
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    prog
  }
}
