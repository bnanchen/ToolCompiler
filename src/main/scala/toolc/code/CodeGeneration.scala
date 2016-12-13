package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {
 
  // A mapping from a parameter/local variable name to the index of this parameter/variable
  // in the fields of a method
  type LocalsPosMapping = Map[String,Int]

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /**** Helper methods ****/
    def generateClassFile(ct: ClassDecl, shortFileName: String, outDir: String): Unit = {
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)
      cf.addDefaultConstructor
      
      // TODO: Add class fields
      cs.members.map{ x => x._2 }.foreach(v => (cf.addField(typeToDescr(v.getType), v.name)))

      // TODO: Add class methods and generate code for them
      // Helper function 
      def translateArguments(argList: List[VariableSymbol]): String = argList match {
        case Nil => ""
        case x :: xs => typeToDescr(x.getType) + translateArguments(xs)
      }
      val listMethods = cs.methods.map{ x => x._2 }.toList
      var i = 0
      while (i < listMethods.size) {
        val ch: CodeHandler =  cf.addMethod(typeToDescr(listMethods(i).getType), listMethods(i).name, translateArguments(listMethods(i).argList)).codeHandler
        cGenMethod(ch, ct.methods(i))
        i += 1
      }
      
      writeClassFile(cf, outDir, cs.name)
    }

    def generateMainClassFile(main: MainObject, sourceFileName: String, outDir: String): Unit = {
      // Main class has a special handling
      val cs = main.getSymbol
      val mainClassFile = new ClassFile(cs.name, None)
      mainClassFile.setSourceFile(sourceFileName)
      mainClassFile.addDefaultConstructor

      cGenMain(
        mainClassFile.addMainMethod.codeHandler,
        prog.main.stats,
        cs.name
      )

      writeClassFile(mainClassFile, outDir, cs.name)
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def writeClassFile(cf: ClassFile, dir: String, className: String) = {
      try {
        cf.writeToFile(dir + className + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }


    def cGenMethod(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // Maps each argument to one local variable index position
      val argMappings = mt.args.zipWithIndex.map { case (arg, index) =>
        arg.id.getSymbol.name -> (index + 1)
      }.toMap

      // Maps each variable to one local variable index position
      val variableMappings = mt.vars.map( v => v.getSymbol.name -> ch.getFreshVar).toMap

      val mapping = argMappings ++ variableMappings

      // TODO: generate code for statements
      mt.stats.foreach { x => cGenStat(x)(ch, mapping, mt.id.value) } 
      
      // TODO: Generate code for the return expression
      cGenExpr(mt.retExpr)(ch, mapping, mt.id.value) 
      
      // TODO: Return with the correct opcode, based on the type of the return expression
      mt.retType.getType match {
        case TInt => ch << IRETURN
        case TBoolean => ch << IRETURN
        case TString => ch << ARETURN
        case TIntArray => ch << ARETURN
        case TClass(cSy) => ch << ARETURN 
        case _ => ch << RETURN 
      }
      ch.freeze
    }

    // Generates code for the main method
    def cGenMain(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      
      // TODO: generate code for main method
      stmts.foreach (x => cGenStat(x)(ch, Map.empty, "Main"))
      ch << RETURN
      ch.freeze
    }


    // Generates code for a statement
    def cGenStat(statement: StatTree)
                (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      statement match {
        case Block(stats) =>
          stats foreach cGenStat
        case If(expr, thn, els) =>
          ch << LineNumber(expr.line)
           cGenExpr(expr)
           val labelFalse = ch.getFreshLabel("ifFalse") 
           val labelNext = ch.getFreshLabel("ifNext")
           ch << IfEq(labelFalse)
           cGenStat(thn)
           ch << Goto(labelNext)
           ch << Label(labelFalse)
           els match {
             case Some(e) => cGenStat(e)
             case None => 
           }
          ch << Label(labelNext)
        case While(expr, stat) => 
          ch << LineNumber(expr.line)
          val labelFalse = ch.getFreshLabel("whileFalse")
          val labelAgain = ch.getFreshLabel("whileAgain")
          ch << Label(labelAgain)
          cGenExpr(expr)
          ch << IfEq(labelFalse)
          cGenStat(stat)
          ch << Goto(labelAgain)
          ch << Label(labelFalse)
        case Println(expr) =>
          ch << LineNumber(expr.line)
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          cGenExpr(expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", "("+ typeToDescr(expr.getType) +")V")
        case Assign(id, expr) =>
          ch << LineNumber(expr.line)
          cGenExpr(expr)
          mapping.get(id.getSymbol.name) match {
            case Some(slotNumber) => 
              // means that it is a local variable because appearing in the mapping
              typeToDescr(expr.getType) match {
                case "I" => ch << IStore(slotNumber)
                case "Z" => ch << IStore(slotNumber)
                case _ => ch << AStore(slotNumber) 
              }
            case None => 
              // means that it is a global variable
              ch << ALoad(0)
              ch << PutField(cname, id.value, typeToDescr(id.getType))
          }
        case ArrayAssign(id, index, expr) => // top: array, index, integer
          ch << LineNumber(expr.line)
          mapping.get(id.getSymbol.name) match {
            case Some(slotNumber) =>
              // means that it is a local variable because appearing in the mapping
              ch << ALoad(slotNumber)
            case None => 
              ch << ALoad(0)
              ch << GetField(cname, id.value, "[I")
          }
          cGenExpr(index)
          cGenExpr(expr)
          ch << IASTORE
        case DoExpr(e) =>
          ch << LineNumber(e.line)
          cGenExpr(e)
          ch << POP // don't need the result on the task, just do the expression e
      }
    }


    // Generates code for an expression
    def cGenExpr(expr: ExprTree)
                (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      expr match {
        case And(lhs,rhs) =>
          ch << ICONST_0 
          cGenExpr(lhs)

          val theLabel = ch.getFreshLabel("alreadyFalse")
          ch << IfEq(theLabel) // consomme le boolean (0 ou 1) retourné par cGenExpr()

          // Only care about the right hand side value
          ch << POP
          cGenExpr(rhs)

          ch << Label(theLabel)
        case Or(lhs, rhs) => {
          ch << ICONST_1            
          cGenExpr(lhs)
          
          val theLabel = ch.getFreshLabel("alreadyTrue")
          ch << IfNe(theLabel)
          
          //Only care about the right hand side value
          ch << POP
          cGenExpr(rhs)
          
          ch << Label(theLabel)
        }
        
        case Not(expr) => {
          cGenExpr(expr) 
          
          val label = ch.getFreshLabel("true")
          val nextLabel = ch.getFreshLabel("next")
          ch << IfEq(label)
          ch << ICONST_0
          ch << Goto(nextLabel)
          
          ch << Label(label)
          ch << ICONST_1
          
          ch << Label(nextLabel)
        }
        
        case Plus(lhs, rhs) => {
          (lhs.getType, rhs.getType) match {
            case (TInt, TInt) => 
              cGenExpr(lhs)
              cGenExpr(rhs) 
              ch << IADD
              
            case (TString, TInt) =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "("+typeToDescr(TString)+")Ljava/lang/StringBuilder;")
              cGenExpr(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()"+ typeToDescr(TString))
            
            case (TInt, TString) =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              cGenExpr(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "("+typeToDescr(TString)+")Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()"+ typeToDescr(TString))
              
            case (TString, TString) =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenExpr(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "("+typeToDescr(TString)+")Ljava/lang/StringBuilder;")
              cGenExpr(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "("+typeToDescr(TString)+")Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()"+ typeToDescr(TString))
              
            case (_, _) => 
              sys.error("You can't addition a "+ lhs.getType +" with a "+ rhs.getType +".")
          }
        }
        
        case Minus(lhs, rhs) => {
        cGenExpr(lhs)
        cGenExpr(rhs)
        ch << ISUB
        }
        
        case Times(lhs, rhs) => {
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << IMUL
        }
        
        case Div(lhs, rhs) => {
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << IDIV
        }
        
        case LessThan(lhs, rhs) => {
          cGenExpr(lhs)
          cGenExpr(rhs)
          val falseLabel = ch.getFreshLabel("notLessThan")
          val nextLabel = ch.getFreshLabel("continue")
          ch << If_ICmpGe(falseLabel) // if_icmpge succeeds if and only if value1 ≥ value2 and go to falseLabel
          ch << ICONST_1
          ch << Goto(nextLabel)
          ch << Label(falseLabel)
          ch << ICONST_0
          ch << Label(nextLabel)
        }
        
        case Equals(lhs, rhs) => {
          cGenExpr(lhs)
          cGenExpr(rhs)
          val falseLabel = ch.getFreshLabel("notEqual")
          val nextLabel = ch.getFreshLabel("continue")
          ch << If_ICmpNe(falseLabel) // if_icmpne succeeds if and only if value1 ≠ value2 and go to falseLabel
          ch << ICONST_1
          ch << Goto(nextLabel)
          ch << Label(falseLabel)
          ch << ICONST_0
          ch << Label(nextLabel)
        }
        
        case ArrayRead(arr, index) => {
          cGenExpr(arr)
          cGenExpr(index)
          ch << IALOAD
        }
        
        case ArrayLength(arr) => {
          cGenExpr(arr)
          ch << ARRAYLENGTH
        }
        
        case NewIntArray(size) => {
          cGenExpr(size) // put the size on top of the stake
          ch << NewArray(10)
        }
        
        case This() => {
          ch << ALoad(0) // push "this" into the stack
        }
        
        case MethodCall(obj, meth, args) => {
          cGenExpr(obj)
          args.foreach { x => cGenExpr(x) }
          val argType: String = 
            (for {
              a <- args
            } yield (typeToDescr(a.getType))).mkString

            ch << InvokeVirtual(obj.getType.toString(), meth.value,"("+ argType +")"+ typeToDescr(meth.getSymbol.getType))
        }
        
        case New(tpe) => {
          ch << DefaultNew(tpe.value)
        }
        
        case IntLit(value) => {
          ch << Ldc(value)
        }
        
        case StringLit(value) => {
          ch << Ldc(value)
        }
        
        case True() => {
          ch << ICONST_1
        }
        
        case False() => {
          ch << ICONST_0
        }
        
        case Variable(id) => {
          mapping.get(id.getSymbol.name) match {
            case Some(slotNumber) =>
              id.getType match {
                case TInt => 
                  ch << ILoad(slotNumber)
                case _ => 
                  ch << ALoad(slotNumber)
              }
            case None => 
              ch << ALoad(0)
              ch << GetField(cname, id.value, typeToDescr(id.getType))
          }
        }
      }

    }

    // Transforms a Tool type to the corresponding JVM type description
    def typeToDescr(t: Type): String = t match {
        case TInt => "I"
        case TBoolean => "Z"
        case TString => "Ljava/lang/String;" 
        case TIntArray => "[I"
        case TClass(cSy) => "L"+ cSy.name +";"
        case _ => sys.error("There is a type problem.")
    }

    /**** Main code ****/

    // Make output directory
    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    // Name of source file (to track positions)
    val sourceName = ctx.files.head.getName

    // output class code
    prog.classes foreach {
      generateClassFile(_, sourceName, outDir)
    }

    // output main object code
    generateMainClassFile(prog.main, sourceName, outDir)

  }   

}

