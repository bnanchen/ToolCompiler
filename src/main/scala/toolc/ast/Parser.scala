package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import grammarcomp.grammar._
import GrammarUtils.InLL1
import CFGrammar._
import grammarcomp.parsing._
import GrammarDSL._

object Parser extends Pipeline[Iterator[Token], Program] {

  val toolGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(), 
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(), 
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'Type ::= INT() ~ LBRACKET() ~ RBRACKET() | BOOLEAN() | INT() | STRING() | 'Identifier,
    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    'Expression ::= 'Expression ~ 'Op ~ 'Expression
      | 'Expression ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | 'Expression ~ DOT() ~ LENGTH()
      | 'Expression ~ DOT() ~ 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN()
      | INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | NEW() ~ 'Identifier ~ LPAREN() ~ RPAREN()
      | BANG() ~ 'Expression
      | LPAREN() ~ 'Expression ~ RPAREN(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
  ))

  // TODO: Transform this to an LL(1) grammar
  val ll1Grammar = Grammar('Program, List[Rules[Token]](
      
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(), 
    
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(), 
    
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(), 
    
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
    
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    
    'Type ::= INT() ~ 'TypeNextInt | BOOLEAN() | STRING() | 'Identifier, // first problématique
    
    'TypeNextInt ::= epsilon() | LBRACKET() ~ RBRACKET(), // par moi
    
    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
      
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
      
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
      
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
      
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    
    'Expression ::= INTLITSENT ~'ExpressionNext | STRINGLITSENT // first problématique and Left Recursion
      | TRUE() ~ 'ExpressionNext | FALSE() ~'ExpressionNext | 'Identifier | THIS() ~'ExpressionNext
      | NEW() ~ 'ExpressionNextNew
      | BANG() ~ 'Expression  ~ 'ExpressionNext
      | LPAREN() ~ 'Expression ~ RPAREN() ~'ExpressionNext,
      
    'ExpressionNextNew ::= INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET() ~ 'ExpressionNext // par moi
      | 'Identifier ~ LPAREN() ~ RPAREN() ~ 'ExpressionNext, 
      
    /*'ExpressionNext ::=  'Op ~ 'Expression ~'ExpressionNext // par moi, stop Left Recursion of 'Expression
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ 'ExpressionNext
      | DOT() ~ 'ExpressionNextDot
      | epsilon(),*/
      
    'ExpressionNext ::= 'Op ~ 'Expression ~ 'ExpressionNext // par moi
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ 'ExpressionNext
      | DOT() ~ 'ExpressionNextDot,
      //| epsilon(),
      
    'ExpressionEpsilon ::= epsilon(), // TODO j'ai le droit de faire ça?
      
    'ExpressionNextDot ::= LENGTH() ~ 'ExpressionNext // TODO est-ce que je peux écrire un non-terminal qui fait référence à un non-terminal plus haut??????????????
      | 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN() ~ 'ExpressionNext,
      
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    
    'ExprList ::= 'Expression ~ 'ExprList,
    
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    
    'Identifier ::= IDSENT
  ))

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()
    val list = tokens.toList
    //val result = grammarcomp.grammar.GrammarUtils.nullableFirstFollow(ll1Grammar)
		//print(result._3)
    GrammarUtils.isLL1WithFeedback(ll1Grammar) match {
      case InLL1() =>
        info("Grammar is in LL1")
      case other =>
        warning(other)
    }
    val feedback = ParseTreeUtils.parseWithTrees(ll1Grammar, list)
    feedback match {
      case s: Success[Token] =>
        (new ASTConstructorLL1).constructProgram(s.parseTrees.head)
      case fdb =>
        fatal("Parsing failed: "+fdb)
    }
  }

}
