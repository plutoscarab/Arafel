module Pretty

open System.CodeDom.Compiler

open Tokens
open Lexer
open Parse
open Print
open Syntax

let rec printAtom (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | NatA(value) ->
        writeSafe writer value
    | StringA(value) ->
        writeSafe writer value
    | OperatorA(symbol) ->
        writeSafe writer symbol
    | LambdaA(lambda) ->
        printLambda writer lambda
    | ParensA(expr) ->
        writer.Write "("
        printExpr writer expr
        writer.Write ")"
    | IdentifierA(name) ->
        writeSafe writer name
    | CasesA(cases) ->
        printCases writer cases
    | IfThenA(ifthen) ->
        printIfThen writer ifthen
    
    writer.Indent <- n

and printCase (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Case(pattern, expr) ->
        printPattern writer pattern
        writer.Write ":"
        writer.Indent <- writer.Indent + 1
        writer.WriteLine ()
        printExpr writer expr
    
    writer.Indent <- n

and printCases (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Cases(expr, cases, otherwise) ->
        writer.Write "case "
        printExpr writer expr
        writer.Write " of"
        writer.Indent <- writer.Indent + 1
        for cases' in cases do
            writer.WriteLine ()
            printCase writer cases'
        match otherwise with
        | None -> ignore()
        | Some otherwise' ->
            writer.WriteLine ""
            writer.Write "else"
            writer.Indent <- writer.Indent + 1
            writer.WriteLine ()
            printExpr writer otherwise'
    
    writer.Indent <- n

and printCommand (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | TypeCmd(typeDecl) ->
        printTypeDecl writer typeDecl
    | LetCmd(letDecl) ->
        printLetDecl writer letDecl
    | ExprCmd(expr) ->
        printExpr writer expr
    
    writer.Indent <- n

and printElseIf (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | ElseIf(condition, trueExpr) ->
        writer.WriteLine ""
        writer.Write "elif "
        printExpr writer condition
        writer.Write " then "
        printExpr writer trueExpr
    
    writer.Indent <- n

and printExpr (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Expr(prelude, atom, args, post) ->
        for prelude' in prelude do
            printPrelude writer prelude'
            writer.WriteLine ()
        printAtom writer atom
        match args with
        | [] -> ignore()
        | _ ->
            writer.Write "("
            match args with
            | [] -> ignore()
            | [args'] ->
                printExpr writer args'
            | args'::args'' ->
                printExpr writer args'
                for args_ in args'' do
                    writer.Write ", "
                    printExpr writer args_
            writer.Write ")"
        for post' in post do
            printPostfix writer post'
    
    writer.Indent <- n

and printIfThen (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | IfThen(condition, trueExpr, elseifs, falseExpr) ->
        writer.Write "if "
        printExpr writer condition
        writer.Write " then "
        printExpr writer trueExpr
        writer.Indent <- writer.Indent + 1
        for elseifs' in elseifs do
            printElseIf writer elseifs'
        writer.WriteLine ""
        writer.Write "else "
        printExpr writer falseExpr
    
    writer.Indent <- n

and printLambda (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Lambda(pattern, expr) ->
        writer.Write "λ "
        printPattern writer pattern
        writer.Write " → "
        printExpr writer expr
    
    writer.Indent <- n

and printLetDecl (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | LetDecl(name, expr) ->
        writer.Write "let "
        printLexpr writer name
        writer.Write " ="
        writer.Indent <- writer.Indent + 1
        writer.WriteLine ()
        printExpr writer expr
        writer.WriteLine ()
    
    writer.Indent <- n

and printLexpr (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Lexpr(name, parameters) ->
        printLexprName writer name
        match parameters with
        | [] -> ignore()
        | _ ->
            writer.Write "("
            match parameters with
            | [] -> ignore()
            | [parameters'] ->
                printLexpr writer parameters'
            | parameters'::parameters'' ->
                printLexpr writer parameters'
                for parameters_ in parameters'' do
                    writer.Write ", "
                    printLexpr writer parameters_
            writer.Write ")"
    
    writer.Indent <- n

and printLexprName (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | IdentifierN(name) ->
        writeSafe writer name
    | OperatorN(symbol) ->
        writeSafe writer symbol
    
    writer.Indent <- n

and printMonoType (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | MonoType(types) ->
        match types with
        | [] -> ignore()
        | [types'] ->
            printLexpr writer types'
        | types'::types'' ->
            printLexpr writer types'
            for types_ in types'' do
                writer.Write " → "
                printLexpr writer types_
    
    writer.Indent <- n

and printPattern (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | CtorPat(ctor, args) ->
        writeSafe writer ctor
        match args with
        | [] -> ignore()
        | _ ->
            writer.Write "("
            match args with
            | [] -> ignore()
            | [args'] ->
                printPattern writer args'
            | args'::args'' ->
                printPattern writer args'
                for args_ in args'' do
                    writer.Write ", "
                    printPattern writer args_
            writer.Write ")"
    | NatPat(value) ->
        writeSafe writer value
    | StringPat(value) ->
        writeSafe writer value
    
    writer.Indent <- n

and printPolyType (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | PolyType(foralls, cases) ->
        match foralls with
        | [] -> ignore()
        | _ ->
            writer.Write "∀ "
            for foralls' in foralls do
                writeSafe writer foralls'
            writer.Write ", "
        match cases with
        | [] -> ignore()
        | [cases'] ->
            printMonoType writer cases'
        | cases'::cases'' ->
            printMonoType writer cases'
            for cases_ in cases'' do
                writer.Write " | "
                printMonoType writer cases_
    
    writer.Indent <- n

and printPostfix (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | SuperscriptPF(value) ->
        let s = toSuperscript value
        writeSafe writer s
    
    writer.Indent <- n

and printPrelude (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | TypeP(typeDecl) ->
        printTypeDecl writer typeDecl
    | LetP(letDecl) ->
        printLetDecl writer letDecl
    
    writer.Indent <- n

and printTypeDecl (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | TypeDecl(name, parameters, ptype) ->
        writer.Write "type "
        writeSafe writer name
        match parameters with
        | [] -> ignore()
        | _ ->
            writer.Write "("
            match parameters with
            | [] -> ignore()
            | [parameters'] ->
                writeSafe writer parameters'
            | parameters'::parameters'' ->
                writeSafe writer parameters'
                for parameters_ in parameters'' do
                    writer.Write ", "
                    writeSafe writer parameters_
            writer.Write ")"
        writer.Write " = "
        printPolyType writer ptype
        writer.WriteLine ()
    
    writer.Indent <- n
