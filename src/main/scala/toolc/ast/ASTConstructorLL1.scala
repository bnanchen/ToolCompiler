package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._

class ASTConstructorLL1 extends ASTConstructor {

  // TODO: Adapt this to your new grammar by overriding/ adding methods as needed
  override def constructProgram(ptree: NodeOrLeaf[Token]): Program = {
    ptree match {
      case Node('Program ::= _, List(mainObj, classDefs, eof)) =>
        val mo = constructMain(mainObj)
        Program(
          mo,
          constructList(classDefs, constructClass)
        ).setPos(mo)
    }
  }

override def constructMain(ptree: NodeOrLeaf[Token]): MainObject = {
  ptree match {
    case Node('MainObject ::= _, List(Leaf(program), objid, _, stmts, _)) =>
      MainObject(constructId(objid), constructList(stmts, constructStatement)).setPos(program)
  }
}

override def constructClass(ptree: NodeOrLeaf[Token]): ClassDecl = {
  ptree match {
    case Node(
        'ClassDeclaration ::= _, 
        List(Leaf(cls), id, optextends, Node('ClassBody ::= _, List(_, vardecls, methoddecls, _)))
        ) => 
          ClassDecl(
              constructId(id),
              constructOption(optextends, constructId),
              constructList(vardecls, constructVarDecl),
              constructList(methoddecls, constructMethodDecl)
              ).setPos(cls)
  }
}

override def constructVarDecl(ptree: NodeOrLeaf[Token]): VarDecl = ptree match {
  case Node('VarDeclaration ::= _, List(Leaf(vr), param, _)) =>
    // Use the parser for parameters which we already have
    val Formal(tpe, id) = constructParam(param)
    VarDecl(tpe, id).setPos(vr)
}

override def constructMethodDecl(ptree: NodeOrLeaf[Token]): MethodDecl = ptree match {
  case Node('MethodDeclaration ::= _, List(Leaf(meth), id, _, params, _, _, tpe, _, _, vardecs, stmts, _, expr, _, _)) =>
    MethodDecl(
        constructId(id),
        constructList(params, constructParam, hasComma = true),
        constructType(tpe),
        constructList(vardecs, constructVarDecl),
        constructList(stmts, constructStatement),
        constructExpr(expr)
        ).setPos(meth)
}

override def constructParam(ptree: NodeOrLeaf[Token]): Formal = {
    ptree match {
      case Node('Param ::= _, List(id, _, tpe)) =>
        val pid = constructId(id)
        Formal(constructType(tpe), pid).setPos(pid)
    }
  }

override def constructId(ptree: NodeOrLeaf[Token]): Identifier = {
    ptree match {
      case Node('Identifier ::= _, List(Leaf(id@ID(name)))) =>
        Identifier(name).setPos(id)
    }
  }

override def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= List(INT(), 'TypeNextInt), List(Leaf(in@INT()), typeNI)) =>
        typeNI match {
          case Node('TypeNextInt ::= List(LBRACKET(), RBRACKET()), List(Leaf(lb), _)) =>
            IntArrayType().setPos(lb)
          case _ => IntType().setPos(in)
        }
      case Node('Type ::= List(BOOLEAN()), List(Leaf(b@BOOLEAN()))) =>
        BooleanType().setPos(b)
      case Node('Type ::= List(STRING()), List(Leaf(s@STRING()))) =>
        StringType().setPos(s)
      case Node('Type ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        ClassType(pid).setPos(pid)
    }
  }


override def constructStatement(ptree: NodeOrLeaf[Token]): StatTree = {
    ptree match {
      case Node('Statement ::= IF() :: _, List(Leaf(iff), _, expr, _, matchif, eopt)) =>
        If(constructExpr(expr), constructStatement(matchif), constructOption(eopt, constructStatement)).setPos(iff)
      case Node('Statement ::= IF() :: _, List(Leaf(iff), _, expr, _, thenif, _, eif)) =>
        If(constructExpr(expr), constructStatement(thenif), Some(constructStatement(eif))).setPos(iff)
      case Node(_ ::= List('SimpleStat), List(simpstat)) =>
        constructStatement(simpstat)
      case Node('SimpleStat ::= LBRACE() :: _, List(Leaf(lbr), stmts, _)) =>
        Block(constructList(stmts, constructStatement)).setPos(lbr)
      case Node('SimpleStat ::= WHILE() :: _, List(Leaf(whl), _, expr, _, stmt)) =>
        While(constructExpr(expr), constructStatement(stmt)).setPos(whl)
      case Node('SimpleStat ::= PRINTLN() :: _, List(Leaf(prln), _, expr, _, _)) =>
        Println(constructExpr(expr)).setPos(prln)
      case Node('SimpleStat ::= DO() :: _, List(Leaf(d), _, expr, _, _)) =>
        DoExpr(constructExpr(expr)).setPos(d)
      case Node('SimpleStat ::= rhs, List(id, idstat)) =>
        val pid = constructId(id)
        idstat match {
          case Node(_ ::= EQSIGN() :: _, List(_, expr, _)) =>
            Assign(pid, constructExpr(expr)).setPos(pid)
          case Node(_, List(_, index, _, _, expr, _)) =>
            ArrayAssign(pid, constructExpr(index), constructExpr(expr)).setPos(pid)
        }
    }
  }

override def constructOp(ptree: NodeOrLeaf[Token]): (ExprTree, ExprTree) => ExprTree = {
    ptree match {
      case Node(_, List(Leaf(t))) => (t: @unchecked) match {
        case AND()      => And
        case OR()       => Or
        case EQUALS()   => Equals
        case LESSTHAN() => LessThan
        case PLUS()     => Plus
        case MINUS()    => Minus
        case TIMES()    => Times
        case DIV()      => Div
      }
    }
  }

def leftParseTree(expr: ExprTree, ptree: NodeOrLeaf[Token]): ExprTree = {
  ptree match {
    case Node('ExpressionOrNext ::= List(OR(), 'ExpressionOr), List(Leaf(or), exprOr)) =>
      exprOr match {
        case Node('ExpressionOr ::= List('ExpressionAnd, 'ExpressionOrNext), List(exprA, exprON)) =>
          val o = Or(expr, constructExprAnd(exprA)).setPos(or)
          leftParseTree(o, exprON)
        case _ =>
          Or(expr, constructExpr(exprOr))
      }
    
    case Node('ExpressionAndNext ::= List(AND(), 'ExpressionAnd), List(Leaf(a), exprA)) =>
      exprA match {
        case Node('ExpressionAnd ::= List('ExpressionEquLessThan, 'ExpressionAndNext), List(exprELT, exprAN)) =>
          val and = And(expr, constructExprEquLessThan(exprELT)).setPos(a)
          leftParseTree(and, exprAN)
        case _ => 
          And(expr, constructExprAnd(exprA))
      }
    
    case Node('ExpressionEquLessThanNext ::= List(EQUALS(), 'ExpressionEquLessThan), List(Leaf(equ), exprEquLessThan)) =>
      exprEquLessThan match {
        case Node('ExpressionEquLessThan ::= List('ExpressionPlusMinus, 'ExpressionEquLessThanNext), List(exprPlusMinus, exprEqLTNext)) =>
          val eq = Equals(expr, constructExprPlusMinus(exprPlusMinus)).setPos(equ)
          leftParseTree(eq, exprEqLTNext)
        case _ => 
          Equals(expr, constructExprPlusMinus(exprEquLessThan)).setPos(equ)
      }
    case Node('ExpressionEquLessThanNext ::= List(LESSTHAN(), 'ExpressionEquLessThan), List(Leaf(lt), exprEquLessThan)) =>
      exprEquLessThan match {
        case Node('ExpressionEquLessThan ::= List('ExpressionPlusMinus, 'ExpressionEquLessThanNext), List(exprPlusMinus, exprEqLTNext)) =>
          val lessthan = LessThan(expr, constructExprPlusMinus(exprPlusMinus)).setPos(lt)
          leftParseTree(lessthan, exprEqLTNext)
        case _ =>
          LessThan(expr, constructExprPlusMinus(exprEquLessThan)).setPos(lt)
      }
      
    case Node('ExpressionPlusMinusNext ::= List(MINUS(), 'ExpressionPlusMinus), List(Leaf(min), exprPlusMinus)) =>
      exprPlusMinus match {
        case Node('ExpressionPlusMinus ::= List('ExpressionDivTimes, 'ExpressionPlusMinusNext), List(exprDivTimes, exprPlusMinusNext)) =>
          val m = Minus(expr, constructExprDivTimes(exprDivTimes)).setPos(min)
          leftParseTree(m, exprPlusMinusNext)
        case _ =>
          Minus(expr, constructExprDivTimes(exprPlusMinus)).setPos(min)
      }
    case Node('ExpressionPlusMinusNext ::= List(PLUS(), 'ExpressionPlusMinus), List(Leaf(plus), exprPlusMinus)) =>
      exprPlusMinus match {
        case Node('ExpressionPlusMinus ::= List('ExpressionDivTimes, 'ExpressionPlusMinusNext), List(exprDivTimes, exprPlusMinusNext)) =>
          val p = Plus(expr, constructExprDivTimes(exprDivTimes)).setPos(plus)
          leftParseTree(p, exprPlusMinusNext)
        case _ =>
          Plus(expr, constructExprDivTimes(exprPlusMinus)).setPos(plus)
      }
      
    case Node('ExpressionDivTimesNext ::= List(DIV(), 'ExpressionDivTimes), List(Leaf(div), exprDivTimes)) =>
      exprDivTimes match {
        case Node('ExpressionDivTimes ::= List('ExpressionBang, 'ExpressionDivTimesNext), List(exprBang, exprDivTimesNext)) =>
          val d = Div(expr, constructExprBang(exprBang)).setPos(div)
          leftParseTree(d, exprDivTimesNext)
        case _ =>
          Div(expr, constructExprBang(exprDivTimes)).setPos(div)
      }
    case Node('ExpressionDivTimesNext ::= List(TIMES(), 'ExpressionDivTimes), List(Leaf(times), exprDivTimes)) =>
      exprDivTimes match {
        case Node('ExpressionDivTimes ::= List('ExpressionBang, 'ExpressionDivTimesNext), List(exprBang, exprDivTimesNext)) =>
          val t = Times(expr, constructExprBang(exprBang)).setPos(times)
          leftParseTree(t, exprDivTimesNext)
        case _ =>
          Times(expr, constructExprBang(exprDivTimes)).setPos(times)
      }
      
      
    case _ => expr
    
    
  }
}

override def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('ExpressionOr ::= List('ExpressionAnd, 'ExpressionOrNext), List(exprAnd, exprOrN)) =>
        val pExprAnd = constructExprAnd(exprAnd)
          leftParseTree(pExprAnd, exprOrN) 
}
}

def constructExprAnd(ptree: NodeOrLeaf[Token]): ExprTree = {
  ptree match {
    case Node('ExpressionAnd ::= List('ExpressionEquLessThan, 'ExpressionAndNext), List(exprEqLT, exprAndN)) =>
      val pExprEqLT = constructExprEquLessThan(exprEqLT)
      leftParseTree(pExprEqLT, exprAndN)
  }
}

def constructExprEquLessThan(ptree: NodeOrLeaf[Token]): ExprTree = {
  ptree match {
    case Node('ExpressionEquLessThan ::= List('ExpressionPlusMinus ,'ExpressionEquLessThanNext), List(exprPM, exprELT)) =>
      val pExprPM = constructExprPlusMinus(exprPM)
      leftParseTree(pExprPM, exprELT)
  }
}

def constructExprPlusMinus(ptree: NodeOrLeaf[Token]): ExprTree = {
  ptree match {
    case Node('ExpressionPlusMinus ::= List('ExpressionDivTimes, 'ExpressionPlusMinusNext), List(exprDT, exprPMN)) =>
      val pExprDT = constructExprDivTimes(exprDT)
      leftParseTree(pExprDT, exprPMN)
  }
}

def constructExprDivTimes(ptree: NodeOrLeaf[Token]): ExprTree = {
  ptree match {
    case Node('ExpressionDivTimes ::= List('ExpressionBang, 'ExpressionDivTimesNext), List(exprB, exprDTN)) =>
      val pExprB = constructExprBang(exprB)
      leftParseTree(pExprB, exprDTN)
  }
}

def constructExprBang(ptree: NodeOrLeaf[Token]): ExprTree = { // pas besoin de passer dans leftParseTree car Bang va toujours devant
  ptree match {
    case Node('ExpressionBang ::= List(BANG(), 'ExpressionBracket), List(Leaf(b), exprBr)) =>
      val pExprBr = constructExprBracket(exprBr)
      Not(pExprBr).setPos(b)
    case Node('ExpressionBang ::= List('ExpressionBracket), List(exprBr)) =>
      constructExprBracket(exprBr)
  }
}

def constructExprBracket(ptree: NodeOrLeaf[Token]): ExprTree = {
  ptree match {
    case Node('ExpressionBracket ::= List('ExpressionDot, 'ExpressionBracketNext), List(exprD, exprBN)) =>
      val pExprD = constructExprDot(exprD)
      val bn = (
          exprBN match {
            case Node('ExpressionBracketNext ::= List(LBRACKET(), 'ExpressionOr, RBRACKET()), List(Leaf(lb), exprOpt, Leaf(rb))) =>
              val pExprOpt: ExprTree = constructExpr(exprOpt) 
              Some(ArrayRead(pExprD, pExprOpt).setPos(lb))
            case _ => None 
          })
      if (bn.isDefined) {
        bn.get 
      } else {
        pExprD
      }
  }
}

def constructExprDotNext(ptree: NodeOrLeaf[Token], obj: ExprTree): ExprTree = {
  ptree match {
    case Node('ExpressionDotNext ::= List(DOT(), 'ExpressionDotNextFollow), List(Leaf(d), exprDotNextFollow)) =>
      exprDotNextFollow match {
        case Node('ExpressionDotNextFollow ::= List('Identifier, LPAREN(), 'Args, RPAREN(), 'ExpressionDotNext), List(id, _, args, _, exprDotNext)) =>
          val pid = constructId(id)
          val a = constructList(args, constructExpr, hasComma = true)
          val expr = MethodCall(obj, pid, a).setPos(d) 
          exprDotNext match {
            case Node('ExpressionDotNext ::= List(DOT(), 'ExpressionDotNextFollow), List(Leaf(dot), exprDNF)) =>
              constructExprDotNext(exprDotNext, expr)
            case _ => 
              expr
          }
        case Node('ExpressionDotNextFollow ::= List(LENGTH()), List(Leaf(len))) =>
          ArrayLength(obj).setPos(len)
      }
  }
}

def constructExprDot(ptree: NodeOrLeaf[Token]): ExprTree = {
  ptree match {
    case Node('ExpressionDot ::= List('ExpressionNew, 'ExpressionDotNext), List(exprNew, exprDotNext)) =>
      val pExprNew = constructExprNew(exprNew)
      val n = ( //constructExprDotNext(exprDotNext, pExprNew)
      exprDotNext match {
        case Node('ExpressionDotNext ::= List(DOT(), 'ExpressionDotNextFollow), List(Leaf(d), exprDotNextFollow)) =>
          Some(constructExprDotNext(exprDotNext, pExprNew))
        case _ => None
      })
     if (n.isDefined) {
       n.get
     } else {
       pExprNew
     }
  }
}

def constructExprNew(ptree: NodeOrLeaf[Token]): ExprTree = {
  ptree match {
  case Node('ExpressionNew ::= List('ExpressionNewNext), List(exprNN)) =>
    val nn = (
        exprNN match {
          case Node('ExpressionNewNext ::= List(NEW(), 'ExpressionNewFollow), List(Leaf(n), exprNF)) =>
            exprNF match {
              case Node('ExpressionNewFollow ::= List(INT(), LBRACKET(), 'ExpressionOr, RBRACKET()), List(Leaf(i), _, exprOr, _)) =>
                val exp = constructExpr(exprOr)
                NewIntArray(exp).setPos(n)
              case Node('ExpressionNewFollow ::= List('Identifier, LPAREN(), RPAREN()), List(id, Leaf(lp), _)) =>
                val i = constructId(id)
                New(i).setPos(n)
            }
        })
    nn
  case Node('ExpressionNew ::= List('ExpressionFinal), List(exprF)) =>
    constructExprFinal(exprF)
  }
}

def constructExprFinal(ptree: NodeOrLeaf[Token]): ExprTree = {
  ptree match {
    case Node('ExpressionFinal ::= List(TRUE()), List(Leaf(t@TRUE()))) =>
      True().setPos(t)
    case Node('ExpressionFinal ::= List(FALSE()), List(Leaf(f@FALSE()))) =>
      False().setPos(f)
    case Node('ExpressionFinal ::= List(LPAREN(), 'ExpressionOr, RPAREN()), List(Leaf(lp), exprOpt, _)) => // pas du tout sûr !!!!!!!!!!!!!!!!!!!!!
      constructExpr(exprOpt)
    case Node('ExpressionFinal ::= List(THIS()), List(Leaf(t@THIS()))) =>
      This().setPos(t)
    case Node('ExpressionFinal ::= List('Identifier), List(id)) =>
      val pid = constructId(id)
      Variable(pid).setPos(pid)
    case Node('ExpressionFinal ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
      StringLit(s).setPos(st)
    case Node('ExpressionFinal ::= List(INTLITSENT), List(Leaf(in@INTLIT(i)))) =>
      IntLit(i).setPos(in)
  }
}

  /** Extracts a List of elements of a generic type A, possibly separated by commas,
    * from a parse tree, by repeating a given parser.
    *
    * The form of the parse tree has to be specific: (t, ts) if there is no
    * comma, and (COMMA(), t, ts) if there is a comma, where t is the tree corresponding
    * to the first element and ts to the rest. Thankfully, this is the case every time
    * we need to parse a List in Tool.
    *
    * @param ptree The input parse tree
    * @param constructor A transformer for an individual object
    * @param hasComma Whether the elements of the list are separated by a COMMA()
    * @tparam A The type of List elements
    * @return A list of parsed elements of type A
    */
override def constructList[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A, hasComma: Boolean = false): List[A] = {
    ptree match {
      case Node(_, List()) => List()
      case Node(_, List(t, ts)) =>
        constructor(t) :: constructList(ts, constructor, hasComma)
      case Node(_, List(Leaf(COMMA()), t, ts)) if hasComma =>
        constructor(t) :: constructList(ts, constructor, hasComma)
    }
  }

  /** Optionally extract an element from a parse tree.
    *
    * The parse tree has to have a specific form: empty production will result in None,
    * and an operator (which will be ignored) followed by the element we need to extract
    * in case of Some.
    *
    * @param ptree The input parse tree
    * @param constructor The extractor of the element if it is present
    * @tparam A The type of the element
    * @return The element wrapped in Some(), or None if the production is empty.
    */
override def constructOption[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A): Option[A] = {
    ptree match {
      case Node(_, List()) => None
      case Node(_, List(_, t)) =>
        Some(constructor(t))
    }
  }
}

