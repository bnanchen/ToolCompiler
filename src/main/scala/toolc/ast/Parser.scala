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

    // On décompose tout pour faire respecter la precedence des opérations
    // on utilise la left factorization

    'ExpressionOptional ::= 'ExpressionOr | epsilon(),

    'ExpressionOr ::= 'ExpressionAnd ~ 'ExpressionOrNext, // d'abord le moins prioritaire, car on veut qu'il soit le plus haut dans l'arbre

    'ExpressionOrNext ::= OR() ~ 'ExpressionOr | epsilon(), //Expr peut être toute une suite d'opérations, d'abord le least prioritaire

    'ExpressionAnd ::= 'ExpressionEquLessThan ~ 'ExpressionAndNext,

    'ExpressionAndNext ::= AND() ~ 'ExpressionAnd | epsilon(),

    'ExpressionEquLessThan ::= 'ExpressionPlusMinus ~ 'ExpressionEquLessThanNext,

    'ExpressionEquLessThanNext ::= EQUALS() ~ 'ExpressionEquLessThan | LESSTHAN() ~ 'ExpressionEquLessThan | epsilon(), // à cause de la precedence des operations

    'ExpressionPlusMinus ::= 'ExpressionDivTimes ~ 'ExpressionPlusMinusNext, // à cause de la precedence des operations

    'ExpressionPlusMinusNext ::= MINUS() ~ 'ExpressionPlusMinus | PLUS() ~ 'ExpressionPlusMinus | epsilon(),

    'ExpressionDivTimes ::= 'ExpressionBang ~ 'ExpressionDivTimesNext, // à cause de la precedence des operations

    'ExpressionDivTimesNext ::= DIV() ~ 'ExpressionDivTimes | TIMES() ~ 'ExpressionDivTimes | epsilon(),

    'ExpressionBang ::= BANG() ~ 'ExpressionBracket | 'ExpressionBracket, // soit il y a un bang devant, soit il y en a pas.

    'ExpressionBracket ::= 'ExpressionDot ~ 'ExpressionBracketNext,

    'ExpressionBracketNext ::= LBRACKET() ~ 'ExpressionOr ~ RBRACKET() | epsilon(), // on peut enlever ExpressionOptional car gérer dans le cas du 'ExpressionNewFollow

    'ExpressionDot ::= 'ExpressionNew ~ 'ExpressionDotNext,

    'ExpressionDotNext ::= DOT() ~ 'ExpressionDotNextFollow | epsilon(),

    'ExpressionDotNextFollow ::= 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN() ~ 'ExpressionDotNext | LENGTH(), //'ExpressionDot

    'ExpressionNew ::= 'ExpressionNewNext | 'ExpressionFinal,

    'ExpressionNewNext ::= NEW() ~ 'ExpressionNewFollow,

    'ExpressionNewFollow ::= INT() ~ LBRACKET() ~ 'ExpressionOr ~ RBRACKET() | 'Identifier ~ LPAREN()  ~ RPAREN(),

    'ExpressionFinal ::= TRUE() | FALSE() | LPAREN() ~ 'ExpressionOr ~ RPAREN() | THIS() | 'Identifier | STRINGLITSENT | INTLITSENT,

    'Args ::= epsilon() | 'ExpressionOr ~ 'ExprList,

    'ExprList ::= COMMA() ~ 'ExpressionOr ~ 'ExprList | epsilon(),

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
