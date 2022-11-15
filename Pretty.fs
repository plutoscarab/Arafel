module Pretty

open System.CodeDom.Compiler

open Language
open Tokens
open Lexer
open Parse

let rec printPostfix (writer:IndentedTextWriter) =
    function
    | SuperscriptPF(f0) ->
        writer.Write f0

and printIfThen (writer:IndentedTextWriter) =
    function
    | IfThen(f0, f1, f2) ->
        writer.Write "if "
        printExpr writer f0
        writer.Write " then "
        printExpr writer f1
        writer.Write " else "
        printExpr writer f2

and printCase (writer:IndentedTextWriter) =
    function
    | Case(f0, f1) ->
        printPattern writer f0
        writer.Write " -> "
        printStatement writer f1

and printMatches (writer:IndentedTextWriter) =
    function
    | Matches(f0, f1, f2) ->
        writer.Write "case "
        printExpr writer f0
        for f1' in f1 do
            printCase writer f1'
        match f2 with
        | None -> ignore()
        | Some f2' ->
            writer.Write " else "
            printExpr writer f2'

and printPattern (writer:IndentedTextWriter) =
    function
    | CtorPat(f0, f1) ->
        writer.Write f0
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
        writer.Write f0
    | StringPat(f0) ->
        writer.Write f0

and printLambda (writer:IndentedTextWriter) =
    function
    | Lambda(f0, f1) ->
        writer.Write "("
        printPattern writer f0
        writer.Write ")"
        writer.Write " = "
        printExpr writer f1

and printAtom (writer:IndentedTextWriter) =
    function
    | NatA(f0) ->
        writer.Write f0
    | StringA(f0) ->
        writer.Write f0
    | OperatorA(f0) ->
        writer.Write f0
    | LambdaA(f0) ->
        printLambda writer f0
    | ParensA(f0) ->
        writer.Write "("
        printExpr writer f0
        writer.Write ")"
    | IdentifierA(f0) ->
        writer.Write f0
    | MatchA(f0) ->
        printMatches writer f0
    | IfThenA(f0) ->
        printIfThen writer f0

and printExpr (writer:IndentedTextWriter) =
    function
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

and printMonoType (writer:IndentedTextWriter) =
    function
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

and printPolyType (writer:IndentedTextWriter) =
    function
    | PolyType(f0, f1) ->
        match f0 with
        | [] -> ignore()
        | _ ->
            writer.Write "forall "
            for f0' in f0 do
                writer.Write f0'
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

and printTypeDecl (writer:IndentedTextWriter) =
    function
    | TypeDecl(f0, f1, f2) ->
        writer.Write "\r\n    type "
        writer.Write f0
        match f1 with
        | [] -> ignore()
        | _ ->
            writer.Write "("
            match f1 with
            | [] -> ignore()
            | [f1'] ->
                writer.Write f1'
            | f1'::f1'' ->
                writer.Write f1'
                for f1_ in f1'' do
                    writer.Write ", "
                    writer.Write f1_
            writer.Write ")"
        writer.Write " = "
        printPolyType writer f2

and printLexprName (writer:IndentedTextWriter) =
    function
    | IdentifierN(f0) ->
        writer.Write f0
    | OperatorN(f0) ->
        writer.Write f0

and printLexpr (writer:IndentedTextWriter) =
    function
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

and printLetDecl (writer:IndentedTextWriter) =
    function
    | LetDecl(f0, f1) ->
        writer.Write "\r\n    let "
        printLexpr writer f0
        writer.Write " = "
        printStatement writer f1

and printPrelude (writer:IndentedTextWriter) =
    function
    | LetP(f0) ->
        printLetDecl writer f0
    | TypeP(f0) ->
        printTypeDecl writer f0
    | CommentP(f0) ->
        writer.Write f0

and printStatement (writer:IndentedTextWriter) =
    function
    | Statement(f0, f1) ->
        for f0' in f0 do
            printPrelude writer f0'
        printExpr writer f1
        writer.WriteLine ()
