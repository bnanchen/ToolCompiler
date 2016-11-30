package toolc

import utils._
import java.io.File

import lexer._
import ast._
import eval._
import analyzer._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {
    val reporter = new Reporter()
    var files: List[File] = Nil
    var outDir: Option[File] = None

    def rec(args: List[String]): Unit = args match {
      case "-d" :: dir :: xs =>
        outDir = Some(new File(dir))
        rec(xs)

      case f :: xs =>
        files  ::= new File(f)
        rec(xs)

      case _ =>
    }

    rec(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, files = files, outDir = outDir)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)
//<<<<<<< HEAD
/*<<<<<<< HEAD
/*<<<<<<< HEAD
    val pipeline = Lexer andThen
                   DisplayTokens
    pipeline.run(ctx)(ctx.files.head)
    ctx.reporter.terminateIfErrors()
=======*/
   /* val pipeline = Lexer andThen Parser
    val ast = pipeline.run(ctx)(ctx.files.head)
    ctx.reporter.terminateIfErrors()
    println(Printer(ast))*/
//>>>>>>> origin/Lab03
    val pipeline = Lexer andThen Parser andThen NameAnalysis
    val ast = pipeline.run(ctx)(ctx.files.head)
    ctx.reporter.terminateIfErrors()
    println(Printer(ast, true))
//>>>>>>> origin/Lab04
=======*/
    /*val pipeline = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking
    val ast = pipeline.run(ctx)(ctx.files.head)
    ctx.reporter.terminateIfErrors()*/
//>>>>>>> origin/Lab05
  //}
//=======
//>>>>>>> origin/Lab06

    val pipeline = Lexer andThen
                   Parser andThen
                   NameAnalysis andThen
                   TypeChecking andThen
                   CodeGeneration

    pipeline.run(ctx)(ctx.files.head)

    ctx.reporter.terminateIfErrors
  }
}
