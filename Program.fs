open System
open System.CodeDom.Compiler
open System.Globalization
open System.IO
open System.Linq
open System.Numerics
open System.Text

let rec build (writer:IndentedTextWriter) expr depth param =
    let indent =
        match expr with
        | Ebnf.Parens _ -> 0
        | _ -> 1
    writer.Indent <- writer.Indent + indent
    let prefix = char (depth + int 'a')
    let deeper = depth + 1
    let w:(string->unit) = writer.WriteLine
    match expr with
    | Ebnf.Choice list ->
        w $"let mutable {prefix}r = NoMatch"
        for item in list do
            w $"{prefix}r <-"
            build writer item deeper param
            w $"if {prefix}r <> NoMatch then {prefix}r else"
        writer.WriteLine "NoMatch"
    | Ebnf.Sequence list ->
        w "let mutable list = []"
        w $"let mutable {prefix}r = NoMatch"
        w $"let mutable {prefix}q = {param}"
        for item in list do
            w $"{prefix}r <-"
            build writer item deeper $"{prefix}q"
            w $"if {prefix}r = NoMatch then NoMatch else"
            w $"match {prefix}r with"
            w $"| NoMatch -> ()"
            w $"| Ok (tree, next) ->"
            w $"    list <- tree :: list"
            w $"    {prefix}q <- next"
        w $"Ok (parseTreeFromList (List.rev list), {prefix}q)"
    | Ebnf.Primary (mult, subexpr) ->
        match mult with
        | Ebnf.ZeroOrOne ->
            w $"let {prefix}r ="
            build writer subexpr deeper param
            w $"match {prefix}r with"
            w $"| NoMatch -> Ok (Empty, {param})"
            w $"| Ok (tree, next) -> Ok (tree, next)"
        | Ebnf.ZeroOrMore ->
            w $"let rec z list ({prefix}q:TokenCursor) ="
            writer.Indent <- writer.Indent + 1
            w $"let {prefix}r ="
            build writer subexpr deeper $"{prefix}q"
            w $"match {prefix}r with"
            w $"| NoMatch -> Ok (parseTreeFromList (List.rev list), {prefix}q)"
            w $"| Ok (tree, next) -> z (tree :: list) next"
            writer.Indent <- writer.Indent - 1
            w $"z [] {param}"
        | Ebnf.OneOrMore ->
            w $"let rec z list ({prefix}q:TokenCursor) ="
            writer.Indent <- writer.Indent + 1
            w $"let {prefix}r ="
            build writer subexpr deeper $"{prefix}q"
            w $"match {prefix}r with"
            w $"| NoMatch -> Ok (parseTreeFromList (List.rev list), {prefix}q)"
            w $"| Ok (tree, next) -> z (tree :: list) next"
            writer.Indent <- writer.Indent - 1
            w $"match z [] {param} with"
            w "| NoMatch -> NoMatch"
            w $"| Ok (Empty, _) -> NoMatch"
            w $"| Ok (Node [], _) -> NoMatch"
            w $"| Ok (tree, next) -> Ok (tree, next)"
    | Ebnf.Parens subexpr ->
        build writer subexpr depth param
    | Ebnf.StringLiteral s ->
        w $"if tokenText ({param}.Current) = \"{s}\" then Ok (Token {param}.Current, {param}.Next) else NoMatch"
    | Ebnf.NcName n -> 
        if n = n.ToUpperInvariant() then
            w $"match {param}.Current with"
            w $"| {n.Substring(0, 1) + n.Substring(1).ToLowerInvariant()} x -> Ok (Token {param}.Current, {param}.Next)"
            w $"| _ -> NoMatch"
        else
            w $"{n} {param}"
    writer.Indent <- writer.Indent - indent

let main =
    let grammar =
        File.ReadAllLines "core.grammar.txt"
        |> Seq.map Ebnf.parseProduction
        |> Map.ofSeq

    let writer = new IndentedTextWriter (File.CreateText "Core.fs")
    writer.WriteLine "module Core"
    writer.WriteLine "// Generated code. Do not edit."
    writer.WriteLine "// Doing this instead of parser combinators for more straightforward debugging."
    writer.WriteLine ()
    writer.WriteLine "open Lexer"
    writer.WriteLine "open Parse"
    writer.WriteLine ()
    let mutable prefix = "let rec"

    for production in grammar do
        writer.WriteLine $"{prefix} {production.Key} (q:TokenCursor) ="
        writer.Indent <- writer.Indent + 1
        writer.WriteLine "let result ="
        build writer production.Value 0 "q"
        writer.WriteLine "match result with"
        writer.WriteLine "| NoMatch -> NoMatch"
        writer.WriteLine $"| Ok (tree, next) -> Ok (Production (\"{production.Key}\", tree), next)"
        writer.Indent <- writer.Indent - 1
        writer.WriteLine ()
        prefix <- "and"

    writer.Flush ()

    Console.OutputEncoding <- Encoding.Unicode
    let src = File.ReadAllText("core.af")
    let cursor = Lexer.makeCursor src
    let tokens = Lexer.tokenise cursor |> Seq.toArray
    let mutable tc = { Lexer.TokenCursor.source = tokens; Lexer.TokenCursor.index = 0 }

    while tc.More do
        let result = Core.expr tc
        match result with
        | Parse.NoMatch -> raise (Exception $"failed at token {tc.index}")
        | Parse.Ok (tree, next) -> tc <- next
    0

