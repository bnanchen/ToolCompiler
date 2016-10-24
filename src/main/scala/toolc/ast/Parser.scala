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
    
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'ExpressionOr ~ SEMICOLON() ~ RBRACE(),
    
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    
    'Type ::= INT() ~ 'TypeNextInt | BOOLEAN() | STRING() | 'Identifier, // first problématique
    
    'TypeNextInt ::= epsilon() | LBRACKET() ~ RBRACKET(), // par moi
    
    'Statement ::= IF() ~ LPAREN() ~ 'ExpressionOr ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
      
    'MatchedIf ::= IF() ~ LPAREN() ~ 'ExpressionOr ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
      
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'ExpressionOr ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'ExpressionOr ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'ExpressionOr ~ RPAREN() ~ SEMICOLON(),
      
    'IdStat ::= EQSIGN() ~ 'ExpressionOr ~ SEMICOLON()
      | LBRACKET() ~ 'ExpressionOr ~ RBRACKET() ~ EQSIGN() ~ 'ExpressionOr ~ SEMICOLON(),
      
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    
    // j'enlève même Expression et ExpressionNext
    // on utilise la left factorization
    
    'ExpressionOptional ::= 'ExpressionOr | epsilon(),
    
    'ExpressionOr ::= 'ExpressionAnd ~ 'ExpressionOrNext, // d'abord le moins prioritaire, car on veut qu'il soit le plus haut dans l'arbre
    
    'ExpressionOrNext ::= OR() ~ 'ExpressionOr | epsilon(), //Expr peut être toute une suite d'opérations, d'abord le least prioritaire
    
    'ExpressionAnd ::= 'ExpressionEqu ~ 'ExpressionAndNext,
    
    'ExpressionAndNext ::= AND() ~ 'ExpressionAnd | epsilon(),
    
    'ExpressionEqu ::= 'ExpressionLessThan ~ 'ExpressionEquNext,
    
    'ExpressionEquNext ::= EQUALS() ~ 'ExpressionEqu | epsilon(),
    
    'ExpressionLessThan ::= 'ExpressionMinus ~ 'ExpressionLessThanNext,
    
    'ExpressionLessThanNext ::= LESSTHAN() ~ 'ExpressionLessThan | epsilon(),
    
    'ExpressionMinus ::= 'ExpressionPlus ~ 'ExpressionMinusNext,
    
    'ExpressionMinusNext ::= MINUS() ~ 'ExpressionMinus | epsilon(), 
    
    'ExpressionPlus ::= 'ExpressionDiv ~ 'ExpressionPlusNext,
    
    'ExpressionPlusNext ::= PLUS() ~ 'ExpressionPlus | epsilon(),
    
    'ExpressionDiv ::= 'ExpressionTimes ~ 'ExpressionDivNext,
    
    'ExpressionDivNext ::= DIV() ~ 'ExpressionDiv | epsilon(),
    
    'ExpressionTimes ::= 'ExpressionBang ~ 'ExpressionTimesNext,
    
    'ExpressionTimesNext ::= TIMES() ~ 'ExpressionTimes | epsilon(), 
    
    'ExpressionBang ::= BANG() ~ 'ExpressionBracket | 'ExpressionBracket, // soit il y a un bang devant, soit il y en a pas. 
    
    'ExpressionBracket ::= 'ExpressionDot ~ 'ExpressionBracketNext,
    
    'ExpressionBracketNext ::= LBRACKET() ~ 'ExpressionBracketInside | epsilon(),
    
    'ExpressionBracketInside ::= 'ExpressionOptional ~ RBRACKET(), 
    
    'ExpressionDot ::= 'ExpressionNew ~ 'ExpressionDotNext, 
    // En haut, le premier Identifier == Expression !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    'ExpressionDotNext ::= /*'ExpressionOr/*'Identifier*/*/ DOT() ~ 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN() ~ 'ExpressionDot | epsilon(), 
    
    'ExpressionNew ::= 'ExpressionFinal ~ 'ExpressionNewNext,
    
    'ExpressionNewNext ::= NEW() ~ 'ExpressionNewFollow | epsilon(),
    
    'ExpressionNewFollow ::= INT() ~ LBRACKET() ~ 'ExpressionOr ~ RBRACKET() | 'Identifier ~ LPAREN()  ~ RPAREN(), 
    
    'ExpressionFinal ::= TRUE() | FALSE() | LPAREN() ~ 'ExpressionOr ~ RPAREN() | THIS() | 'Identifier | STRINGLITSENT | INTLITSENT,
     
    // END 
    /*
    'Expression ::= INTLITSENT ~'ExpressionNext | STRINGLITSENT // first problématique and Left Recursion, tu le décomposes!
      | TRUE() ~ 'ExpressionNext | FALSE() ~'ExpressionNext | 'Identifier | THIS() ~'ExpressionNext
      | NEW() ~ 'ExpressionNextNew
      | BANG() ~ 'Expression  ~ 'ExpressionNext // only place with BANG()
      | LPAREN() ~ 'Expression ~ RPAREN() ~'ExpressionNext,
      
    'ExpressionNextNew ::= INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET() ~ 'ExpressionNext // par moi
      | 'Identifier ~ LPAREN() ~ RPAREN() ~ 'ExpressionNext, 
      
    /*'ExpressionNext ::=  'Op ~ 'Expression ~'ExpressionNext // par moi, stop Left Recursion of 'Expression
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ 'ExpressionNext
      | DOT() ~ 'ExpressionNextDot
      | epsilon(),*/
      
    'ExpressionNext ::= 'Op ~ 'Expression ~ 'ExpressionNext // par moi
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ 'ExpressionNext
      | DOT() ~ 'ExpressionNextDot
      | epsilon(), 
      
    'ExpressionNextDot ::= LENGTH() ~ 'ExpressionNext // par moi
      | 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN() ~ 'ExpressionNext,
      */
    'Args ::= epsilon() | 'ExpressionOr ~ 'ExprList,
    
    'ExprList ::= 'ExpressionOr ~ 'ExprList,
    
    //'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    
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
