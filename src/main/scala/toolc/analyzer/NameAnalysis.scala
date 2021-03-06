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
       
      // Create empty symbols for all classes, checking that their names are unique
      for (c <- prog.classes) {
        // verify
        if (global.classes.contains(c.id.value)) {
          error("Two classes can't have the same name.")
        } else if (c.id.value == "Object") {
            error("No classes can't be named 'Object'.")
          } else if (c.id.value == prog.main.id.value) {
            error("A class can't have the same name as the Main Object.")
          } else if (prog.main.id.value == "Object") {
            error("The Main Object can't be named 'Object'")
          }
          val clSym = new ClassSymbol(c.id.value).setPos(c) 
          c.setSymbol(clSym)
          c.id.setSymbol(clSym)
          global.classes = global.classes.+((c.id.value, clSym))
        
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
        //       Traverse a class to collect symbols and emit errors
        //       in case a correctness rule of Tool is violated
        // Note: It is important that you analyze parent classes first 
        // first the parents
        def acc(klass: Option[Identifier]): List[Option[ClassSymbol]] = klass match {
          case Some(cl) => {
            val classDec = prog.classes.filter{ (x: ClassDecl) => (x.id.value==cl.value) }(0)
            collectInClass(classDec)
            global.lookupClass(cl.value) :: acc(classDec.parent)
          }
          case None => Nil
        }
        val toOverride = acc(c.parent)
        
        // first I collect the methods of the class:
        for (m <- c.methods) {
          val mSym = new MethodSymbol(m.id.value, global.classes(c.id.value)).setPos(m) 
          for (p <- m.vars) {
            setTypeSymbol(p.tpe, global)
            val varSym = new VariableSymbol(p.id.value)
            varSym.setType(p.tpe.getType) // ajouté durant le labo Type Checking 
            mSym.members = mSym.members + (p.id.value -> varSym.setPos(p))
            // verify that two members have not the same name
            if (m.vars.filter { x => (x.id.value==p.id.value)}.size != 1) {
              error("Two members of one method can't have the same name.")
            }
          }
          
          for (a <- m.args) {
            setTypeSymbol(a.tpe, global)
            val aSym = new VariableSymbol(a.id.value).setPos(a)
            aSym.setType(a.tpe.getType) // ajouté durant le labo Type Checking
            mSym.params = mSym.params + (a.id.value -> aSym)
            mSym.argList = mSym.argList :+ aSym
            // verify that two arguments have note the same name
            if (m.args.filter { x => (x.id.value==a.id.value) }.size != 1) {
              error("Two arguments of one method can't have the same name.")
            }
          }
          // verify 
          if (m.args.map{ x => x.id.value }.intersect(m.vars.map{ x => x.id.value }).size != 0) {
            error("In a method, two parameters/local variables can't have the same name.")
          }
          // set the type of the return therefore of the method
          setTypeSymbol(m.retType, global)
          mSym.setType(m.retType.getType) // ajouté durant le labo Type Checking 
          def controlMethodOverride(toOverride: Option[ClassSymbol]) = toOverride match {
            case None => 
            case Some(clSym) => {
              clSym.methods.get(m.id.value) match {
                case Some(mtdSym) => {
                  if (mtdSym.params.keys.size != mSym.params.keys.size) { // the two methods must have the same number of arguments
                    error("A method can't override another with a different number of parameters.")
                  }
                  if (m.retType.getType.toString() != mtdSym.getType.toString()) {
                    error("A method that overrides another needs to have the same return type.")
                  }
                  var i = 0
                  var listBool: List[Boolean] = Nil
                  while (i < mtdSym.argList.length) {
                    listBool = listBool :+ (mSym.argList(i).getType.isSubTypeOf(mtdSym.argList(i).getType))
                    i += 1
                  }
                  if (listBool.contains(false)) {
                    error("All the arguments of a method that overrides another one need to be subtype of the arguments of the overriden one.")
                  }

                  mSym.overridden = clSym.methods.get(m.id.value)
                }
                case None =>
              }
            }
          }
          toOverride.foreach { x => controlMethodOverride(x) }
            
          // BEGIN 
          m.id.setSymbol(mSym)
          m.setSymbol(mSym)
          // END
          // Add the method to the ClassSymbol corresponding:
          global.classes(c.id.value).methods = global.classes(c.id.value).methods + (m.id.value -> mSym)
        }
        
        
        // addition of the methods of the parent that are not declared:
          c.parent match {
            case Some(id) => {
              def accAddMeth(m: MethodSymbol) = {
                if (global.classes(c.id.value).methods.get(m.name) == None) {
                  global.classes(c.id.value).methods = global.classes(c.id.value).methods + (m.name -> m)
                }
              }
              global.classes(id.value).methods.map{x => x._2}.foreach { y => accAddMeth(y) }
              
            }
            case None =>
          }
        
        // second the variables: 
        for (v <- c.vars) {
          setTypeSymbol(v.tpe, global)
          val vSym = new VariableSymbol(v.id.value).setPos(v)
          vSym.setType(v.tpe.getType) // ajouté durant le labo Type Checking
          // Verify that two variables have not the same name
          if (c.vars.filter { x => (x.id.value==v.id.value) }.size != 1) {
            error("Two variables of one class can't have the same name.")
          }
          // Verify that a variable has not the same name has a variable inside an inherited class:
          def controlVariableOverride(toOverride: Option[ClassSymbol]) = toOverride match {
            case None =>
            case Some(clSym) => {
              if (clSym.members.contains(v.id.value)) {
                error("A variable can't have the same name as a variable inside an inherited class.")
              }
            }
          }
          toOverride.foreach { x => controlVariableOverride(x) }
          
          // Add the variable to the ClassSymbol corresponding: 
          global.classes(c.id.value).members = global.classes(c.id.value).members + (v.id.value -> vSym)
        }
        
      }
      
      global
    }

    def setPSymbols(prog: Program, gs: GlobalScope): Unit = {
      //       Traverse within each definition of the program
      //       and attach symbols to Identifiers and "this"
      prog.classes.foreach(setCSymbols(_,gs))
      prog.main.id.setSymbol(gs.mainClass)
      prog.main.stats.foreach(setSSymbols(_)(gs, None))
    }

    def setCSymbols(klass: ClassDecl, gs: GlobalScope): Unit = {
      val classSym = gs.lookupClass(klass.id.value).get
      for (varDecl <- klass.vars) {
        varDecl.id.setSymbol(classSym.lookupVar(varDecl.id.value).get) // ajouté par moi
        setTypeSymbol(varDecl.tpe, gs)
      }
       
      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol): Unit = {
      val methodSym = cs.lookupMethod(meth.id.value).get
      meth.setSymbol(methodSym)
      meth.id.setSymbol(methodSym)
      for (varDecl <- meth.vars) {
        varDecl.setSymbol(methodSym.lookupVar(varDecl.id.value).get) // ajouté par moi
        varDecl.id.setSymbol(methodSym.lookupVar(varDecl.id.value).get) // ajouté par moi
        setTypeSymbol(varDecl.tpe, gs)
      }
      for (args <- meth.args) {
        args.setSymbol(methodSym.lookupVar(args.id.value).get) // ajouté par moi
        args.id.setSymbol(methodSym.lookupVar(args.id.value).get) // ajouté par moi
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
        case m: MethodCall => {
          // ajouté durant le labo Type Checking:
          setESymbols(m.obj)
          m.args.foreach(setESymbols(_))
          m.obj.getType match {
            case Types.TClass(clSy) => {
              clSy.lookupMethod(m.meth.value) match { 
                case Some(s) => 
                  m.meth.setSymbol(s)
                case None =>
              }
            }
            case _ => 
          }
          m.meth.getSymbol.setType(m.getType) 
        }
        case Variable(id) => setISymbol(id)
        case t: This => { 
          t.setSymbol(ms.get.classSymbol)
        }
        case NewIntArray(size) => setESymbols(size)
        case n: New => { 
          gs.classes.get(n.tpe.value) match {
            case Some(c) => n.tpe.setSymbol(c)
            case None => 
          }
        }
        case Not(expr) => setESymbols(expr)
        case _ => 
      }
    }

    def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = {
      tpe match {
        case c: ClassType => {
          gs.classes.get(c.id.value) match {
            case Some(cl) => { 
              c.id.setSymbol(cl) 
            }
            case None => 
          }
        }
        case _ => 
      }
    }

    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    prog
  }
}
