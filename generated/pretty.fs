module Pretty
// Generated code. Do not edit.

open System.CodeDom.Compiler

open Tokens
open Lexer
open Parse
open Print
open Syntax

let rec printAtom (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | CallA(fn, args) ->
        printAtom writer fn
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
    | ExponentA(atom, exponent) ->
        printAtom writer atom
        let s = toSuperscript exponent
        writeSafe writer s
    | IntSeqA(intSeq) ->
        writer.Write "["
        printIntSeq writer intSeq
        writer.Write "]"
    | NatA(value) ->
        writeSafe writer value
    | StringA(value) ->
        writer.Write "\""
        writeSafe writer value
        writer.Write "\""
    | BoolA(value) ->
        let b = value.ToString().ToLowerInvariant()
        writer.Write b
    | OperatorA(symbol) ->
        writer.Write "["
        writeSafe writer symbol
        writer.Write "]"
    | ParensA(expr) ->
        writer.Write "("
        printExpr writer expr
        writer.Write ")"
    | IdentifierA(name) ->
        writeSafe writer name
    
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
    | Expr(term, terms) ->
        printUnatom writer term
        for terms' in terms do
            printTerm writer terms'
    | TypeE(typeDecl) ->
        printTypeDecl writer typeDecl
    | LetE(letDecl) ->
        printLetDecl writer letDecl
    | CasesE(cases) ->
        printCases writer cases
    | IfThenE(ifthen) ->
        printIfThen writer ifthen
    | LambdaE(name, expr) ->
        writer.Write "af"
        writer.Write "("
        printLexpr writer name
        writer.Write ")"
        writer.Write " = "
        printExpr writer expr
    | CurlyLambdaE(expr) ->
        writer.Write "{"
        printExpr writer expr
        writer.Write "}"
    
    writer.Indent <- n

and printIfThen (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | IfThen(condition, trueExpr, elseifs, falseExpr) ->
        writer.Write "if "
        printExpr writer condition
        writer.Write " then "
        printExpr writer trueExpr
        for elseifs' in elseifs do
            printElseIf writer elseifs'
        writer.WriteLine ""
        writer.Write "else "
        printExpr writer falseExpr
    
    writer.Indent <- n

and printIntSeq (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | IntSeq(ranges) ->
        match ranges with
        | [] -> ignore()
        | _ ->
            match ranges with
            | [] -> ignore()
            | [ranges'] ->
                printRange writer ranges'
            | ranges'::ranges'' ->
                printRange writer ranges'
                for ranges_ in ranges'' do
                    writer.Write ", "
                    printRange writer ranges_
    
    writer.Indent <- n

and printLetDecl (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | LetDecl(name, expr, inExpr) ->
        writer.Write "let "
        printLexpr writer name
        writer.Write " ="
        writer.Indent <- writer.Indent + 1
        writer.WriteLine ()
        printExpr writer expr
        writer.Indent <- writer.Indent - 1
        
        writer.WriteLine ""
        writer.WriteLine ""
        match inExpr with
        | None -> ignore()
        | Some inExpr' ->
            writer.Write "in"
            printExpr writer inExpr'
    
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
    | BoolPat(value) ->
        let b = value.ToString().ToLowerInvariant()
        writer.Write b
    
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

and printRange (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Range(first, last, skip) ->
        writeSafe writer first
        writer.Write ".."
        match last with
        | None -> ignore()
        | Some last' ->
            writeSafe writer last'
        match skip with
        | None -> ignore()
        | Some skip' ->
            writer.Write " by "
            writeSafe writer skip'
    
    writer.Indent <- n

and printTerm (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Term(operator, atom) ->
        writer.Write " "
        writeSafe writer operator
        writer.Write " "
        printUnatom writer atom
    
    writer.Indent <- n

and printTypeDecl (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | TypeDecl(name, parameters, ptype, inExpr) ->
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
        writer.WriteLine ()
        printExpr writer inExpr
    
    writer.Indent <- n

and printUnatom (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Unatom(operator, atom) ->
        match operator with
        | None -> ignore()
        | Some operator' ->
            writeSafe writer operator'
        printAtom writer atom
    
    writer.Indent <- n
