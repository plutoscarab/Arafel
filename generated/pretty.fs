module Pretty

open System.CodeDom.Compiler

open Language
open Tokens
open Lexer
open Parse
open Print

let rec printPostfix (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | SuperscriptPF(f0) ->
        let s = toSuperscript f0
        writeSafe writer s
    | CommentPF(f0) ->
        writer.WriteLine ()
        writeSafe writer f0
    
    writer.Indent <- n

and printIfThen (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | IfThen(f0, f1, f2) ->
        writer.Write "if "
        printExpr writer f0
        writer.Indent <- writer.Indent + 1
        writer.WriteLine ""
        writer.Write "then "
        printExpr writer f1
        writer.WriteLine ""
        writer.Write "else "
        printExpr writer f2
    
    writer.Indent <- n

and printCase (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Case(f0, f1) ->
        printPattern writer f0
        writer.WriteLine ":"
        printStatement writer f1
    
    writer.Indent <- n

and printCases (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Cases(f0, f1, f2) ->
        writer.Write "case "
        printExpr writer f0
        writer.Write " of"
        writer.Indent <- writer.Indent + 1
        for f1' in f1 do
            writer.WriteLine ()
            printCase writer f1'
        match f2 with
        | None -> ignore()
        | Some f2' ->
            writer.WriteLine ""
            writer.WriteLine "else"
            printStatement writer f2'
    
    writer.Indent <- n

and printPattern (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | CtorPat(f0, f1) ->
        writeSafe writer f0
        match f1 with
        | [] -> ignore()
        | _ ->
            writer.Write "("
            match f1 with
            | [] -> ignore()
            | [f1'] ->
                printPattern writer f1'
            | f1'::f1'' ->
                printPattern writer f1'
                for f1_ in f1'' do
                    writer.Write ", "
                    printPattern writer f1_
            writer.Write ")"
    | NatPat(f0) ->
        writeSafe writer f0
    | StringPat(f0) ->
        writeSafe writer f0
    
    writer.Indent <- n

and printLambda (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Lambda(f0, f1) ->
        writer.Write "("
        printPattern writer f0
        writer.Write ")"
        writer.Write " = "
        printExpr writer f1
    
    writer.Indent <- n

and printAtom (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | NatA(f0) ->
        writeSafe writer f0
    | StringA(f0) ->
        writeSafe writer f0
    | OperatorA(f0) ->
        writeSafe writer f0
    | LambdaA(f0) ->
        printLambda writer f0
    | ParensA(f0) ->
        writer.Write "("
        printExpr writer f0
        writer.Write ")"
    | IdentifierA(f0) ->
        writeSafe writer f0
    | CasesA(f0) ->
        printCases writer f0
    | IfThenA(f0) ->
        printIfThen writer f0
    
    writer.Indent <- n

and printExpr (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Expr(f0, f1, f2) ->
        printAtom writer f0
        match f1 with
        | [] -> ignore()
        | _ ->
            writer.Write "("
            match f1 with
            | [] -> ignore()
            | [f1'] ->
                printExpr writer f1'
            | f1'::f1'' ->
                printExpr writer f1'
                for f1_ in f1'' do
                    writer.Write ", "
                    printExpr writer f1_
            writer.Write ")"
        for f2' in f2 do
            printPostfix writer f2'
    
    writer.Indent <- n

and printLetDecl (writer:IndentedTextWriter) value =
    let n = writer.Indent
    writer.Indent <- n + 1
    
    match value with
    | LetDecl(f0, f1) ->
        writer.Write "let "
        printLexpr writer f0
        writer.WriteLine " ="
        printStatement writer f1
        writer.WriteLine ()
    
    writer.Indent <- n

and printLexprName (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | IdentifierN(f0) ->
        writeSafe writer f0
    | OperatorN(f0) ->
        writeSafe writer f0
    
    writer.Indent <- n

and printLexpr (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Lexpr(f0, f1) ->
        printLexprName writer f0
        match f1 with
        | [] -> ignore()
        | _ ->
            writer.Write "("
            match f1 with
            | [] -> ignore()
            | [f1'] ->
                printLexpr writer f1'
            | f1'::f1'' ->
                printLexpr writer f1'
                for f1_ in f1'' do
                    writer.Write ", "
                    printLexpr writer f1_
            writer.Write ")"
    
    writer.Indent <- n

and printMonoType (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | MonoType(f0) ->
        match f0 with
        | [] -> ignore()
        | [f0'] ->
            printLexpr writer f0'
        | f0'::f0'' ->
            printLexpr writer f0'
            for f0_ in f0'' do
                writer.Write " -> "
                printLexpr writer f0_
    
    writer.Indent <- n

and printPolyType (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | PolyType(f0, f1) ->
        match f0 with
        | [] -> ignore()
        | _ ->
            writer.Write "forall "
            for f0' in f0 do
                writeSafe writer f0'
            writer.Write ", "
        match f1 with
        | [] -> ignore()
        | [f1'] ->
            printMonoType writer f1'
        | f1'::f1'' ->
            printMonoType writer f1'
            for f1_ in f1'' do
                writer.Write " | "
                printMonoType writer f1_
    
    writer.Indent <- n

and printTypeDecl (writer:IndentedTextWriter) value =
    let n = writer.Indent
    writer.Indent <- n + 1
    
    match value with
    | TypeDecl(f0, f1, f2) ->
        writer.Write "type "
        writeSafe writer f0
        match f1 with
        | [] -> ignore()
        | _ ->
            writer.Write "("
            match f1 with
            | [] -> ignore()
            | [f1'] ->
                writeSafe writer f1'
            | f1'::f1'' ->
                writeSafe writer f1'
                for f1_ in f1'' do
                    writer.Write ", "
                    writeSafe writer f1_
            writer.Write ")"
        writer.Write " = "
        printPolyType writer f2
        writer.WriteLine ()
    
    writer.Indent <- n

and printPrelude (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | CommentP(f0) ->
        writeSafe writer f0
    | TypeP(f0) ->
        printTypeDecl writer f0
    | LetP(f0) ->
        printLetDecl writer f0
    
    writer.Indent <- n

and printStatement (writer:IndentedTextWriter) value =
    let n = writer.Indent
    
    match value with
    | Statement(f0, f1) ->
        for f0' in f0 do
            printPrelude writer f0'
        writer.Indent <- writer.Indent + 1
        printExpr writer f1
        writer.Indent <- writer.Indent - 1
    
    writer.Indent <- n
