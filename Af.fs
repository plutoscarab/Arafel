open System
open System.CodeDom.Compiler
open System.Globalization
open System.IO
open System.Linq
open System.Numerics
open System.Text

open Reflect
open Parse
open Print
open Ebnf
open Syntax

let rec orLast =
    function
    | [] -> []
    | [s] -> [s]
    | [p; u] -> [p; "or " + u]
    | h::t -> h::(orLast t)

let errStr e t =
    let ex = String.concat ", " (orLast e)

    match t with
    | [] ->
        $"End of text: Expected {ex}."
    | he::_ ->
        let cu = Tokens.tokenCursor he
        let tx = Tokens.tokenText he
        $"Line {cu.line} Pos {cu.pos} «{tx}»: Expected {ex}."

let sanityCheck =

    let src = File.ReadAllText("sample.af", Encoding.UTF8)
    let cursor = Cursor.makeCursor src
    let tokens = Lexer.tokenise Arafel.keywords cursor |> Seq.toList
    use file = File.CreateText("generated/pretty.af")
    use writer = new IndentedTextWriter(file)
    writer.Indent <- 1

    let mutable t = tokens

    while t <> [] do

        let (r, t2) = Arafel.expr t

        let err e t =
            raise (Exception (errStr e t))

        match r with
        | Nomatch e -> err e t
        | SyntaxError e -> err e t2
        | Match e ->
            writer.WriteLine ()
            Pretty.printExpr writer e
            writer.WriteLine ()

        t <- t2

let lines() =
    Seq.initInfinite (fun _ ->
        Console.Write "\naf] "
        Console.ReadLine().Trim())
    |> Seq.takeWhile (fun line -> line <> "quit")

let tokenise (src: string) =
    src
    |> Cursor.makeCursor
    |> Lexer.tokenise Arafel.keywords
    |> Seq.toList

type Value =
    | NatV of bigint
    | StringV of string
    | ErrorV of string
    | NotImplV

type Context =
    { bound : Map<LexprName, Value> }

type Resolution =
    | ContextR of Context
    | ValueR of Value
    | ErrorR of string
    | NotImplR

let rec evalId context id =
    match context.bound.TryFind id with
    | None -> ErrorV $"{id} is not bound to a value."
    | Some v -> v

and evalCases context cases =
    NotImplV

and evalIfThen context ifThen =
    NotImplV

and evalAtom context =
    function
    | NatA n -> NatV n
    | StringA s -> StringV s
    | OperatorA s -> NotImplV
    | LambdaA e -> NotImplV
    | ParensA e -> evalExpr context e
    | IdentifierA id ->
        if Rune.IsLetter(Rune.GetRuneAt(id, 0)) then
            evalId context (IdentifierN id)
        else
            evalId context (OperatorN id)
    | CasesA e -> evalCases context e
    | IfThenA e -> evalIfThen context e

and evalExpr context (Expr(prelude, atom, args, postfix)) =
    let eatom = evalAtom context atom
    eatom

let applyLet context lexpr expr =
    match lexpr with
    | Lexpr (id, []) ->
        match evalId context id with
        | ErrorV _ ->
            match evalExpr context expr with
            | ErrorV e -> ErrorR e
            | NotImplV -> NotImplR
            | v -> ContextR { bound = Map.add id v (context.bound) }
        | _ -> ErrorR $"{id} is already bound to a value."
    | _ -> NotImplR

let apply context =
    function
    | LetCmd (LetDecl(lexpr, expr)) ->
        applyLet context lexpr expr
    | TypeCmd (TypeDecl(id, parms, polytype)) ->
        NotImplR
    | ExprCmd e ->
        match evalExpr context e with
        | ErrorV e -> ErrorR e
        | v -> ValueR v

let execute context src =
    let t = tokenise src
    let (m, t2) = Arafel.command t

    match m with
    | Nomatch e ->
        ErrorR (errStr e t)
    | SyntaxError e ->
        ErrorR (errStr e t2)
    | Match code ->
        use writer = new IndentedTextWriter(Console.Out)
        Pretty.printCommand writer code
        writer.WriteLine ()
        apply context code

let main =

    Console.OutputEncoding <- Encoding.UTF8
    sanityCheck
    Console.WriteLine "Enter 'quit' to quit. End lines with '\\' for multiline."
    let mutable src = ""
    let mutable context = { bound = Map.empty<LexprName, Value> }

    for line in lines() do
        let d = line.EndsWith("\\")
        let c = if d then line.Substring(0, line.Length - 1).Trim() else line
        src <- src + c + " "

        if not d then
            match execute context src with
            | ContextR c -> context <- c
            | ValueR v -> Console.WriteLine (v.ToString())
            | ErrorR e -> Console.WriteLine e
            | NotImplR -> Console.WriteLine "Capability not yet implemented."
            src <- ""
    0