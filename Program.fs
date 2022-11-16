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

    let mutable t = tokens

    while t <> [] do

        let (r, t2) = Arafel.statement t

        let err e t =
            raise (Exception (errStr e t))

        match r with
        | Nomatch e -> err e t
        | SyntaxError e -> err e t2
        | Match e ->
            Pretty.printStatement writer e
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
    | OperatorV of string
    | LambdaV of Lambda

let rec evalId context id =
    raise (NotImplementedException())

and evalCases context cases =
    raise (NotImplementedException())

and evalIfThen context ifThen =
    raise (NotImplementedException())

and evalAtom context =
    function
    | NatA n -> NatV n
    | StringA s -> StringV s
    | OperatorA s -> OperatorV s
    | LambdaA e -> LambdaV e
    | ParensA e -> evalExpr context e
    | IdentifierA id -> evalId context id
    | CasesA e -> evalCases context e
    | IfThenA e -> evalIfThen context e

and evalExpr context (Expr(atom, args, postfix)) =
    let eatom = evalAtom context atom
    eatom

let apply context =
    function
    | LetCmd (LetDecl(lexpr, statement)) ->
        context
    | TypeCmd (TypeDecl(id, parms, polytype)) ->
        context
    | ExprCmd e ->
        let value = evalExpr context e
        context

let execute context src =
    let t = tokenise src
    let (m, t2) = Arafel.command t

    match m with
    | Nomatch e ->
        Console.WriteLine (errStr e t)
        context
    | SyntaxError e ->
        Console.WriteLine (errStr e t2)
        context
    | Match code ->
        use writer = new IndentedTextWriter(Console.Out)
        Pretty.printCommand writer code
        apply context code

let main =

    Console.OutputEncoding <- Encoding.UTF8
    sanityCheck
    Console.WriteLine "Enter 'quit' to quit. End lines with '\\' for multiline."
    let mutable src = ""
    let mutable context = ()

    for line in lines() do
        let d = line.EndsWith("\\")
        let c = if d then line.Substring(0, line.Length - 1).Trim() else line
        src <- src + " " + c

        if not d then
            context <- execute context src
            src <- ""
    0