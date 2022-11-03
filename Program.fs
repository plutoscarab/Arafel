open System
open System.CodeDom.Compiler
open System.Globalization
open System.IO
open System.Linq
open System.Numerics
open System.Text

let rec exprStr expr =
    match expr with
    | Ebnf.Choice list -> "c [" + (String.concat "; " (List.map exprStr list)) + "]"
    | Ebnf.Sequence list -> "s [" + (String.concat "; " (List.map exprStr list)) + "]"
    | Ebnf.Primary (mult, expr) ->
        match mult with
        | Ebnf.ZeroOrOne -> "o (" + (exprStr expr) + ")"
        | Ebnf.ZeroOrMore -> "z (" + (exprStr expr) + ")"
        | Ebnf.OneOrMore -> "m (" + (exprStr expr) + ")"
    | Ebnf.Parens expr -> exprStr expr
    | Ebnf.StringLiteral s -> "l \"" + s + "\""
    | Ebnf.NcName n -> 
        if n = n.ToUpperInvariant() then
            "t " + n.Substring(0, 1) + n.Substring(1).ToLowerInvariant()
        else
            n

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
            w $"| Ok (result, next) ->"
            w $"    list <- result :: list"
            w $"    {prefix}q <- next"
        w $"Ok (Node (List.rev list), {prefix}q)"
    | Ebnf.Primary (mult, subexpr) ->
        match mult with
        | Ebnf.ZeroOrOne ->
            w $"let {prefix}r ="
            build writer subexpr deeper param
            w $"match {prefix}r with"
            w $"| NoMatch -> Ok (Empty, {param})"
            w $"| Ok (result, next) -> Ok (result, next)"
        | Ebnf.ZeroOrMore ->
            w $"let z list ({prefix}q:TokenCursor) ="
            writer.Indent <- writer.Indent + 1
            w $"let {prefix}r ="
            build writer subexpr deeper $"{prefix}q"
            w $"match {prefix}r with"
            w $"| NoMatch -> Ok (Node (List.rev list), {prefix}q)"
            w $"| Ok (result, next) -> z (result :: list) next"
            writer.Indent <- writer.Indent - 1
            w $"z [] {param}"
        | Ebnf.OneOrMore ->
            w $"let z list ({prefix}q:TokenCursor) ="
            writer.Indent <- writer.Indent + 1
            w $"let {prefix}r ="
            build writer subexpr deeper $"{prefix}q"
            w $"match {prefix}r with"
            w $"| NoMatch -> Ok (Node (List.rev list), {prefix}q)"
            w $"| Ok (result, next) -> z (result :: list) next"
            writer.Indent <- writer.Indent - 1
            w $"match z [] {param} with"
            w "| NoMatch -> ignore"
            w $"| Ok (Node [], _) -> NoMatch"
            w $"| Ok (result, next) -> Ok (result, next)"
    | Ebnf.Parens subexpr ->
        build writer subexpr depth param
    | Ebnf.StringLiteral s ->
        w $"if {param}.Current = Id \"{s}\" then Ok (Token {param}.Current, {param}.Next) else NoMatch"
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
    writer.WriteLine ()
    writer.WriteLine "open Lexer"
    writer.WriteLine "open Parse"
    writer.WriteLine ()
    let mutable prefix = "let rec"

    for production in grammar do
        writer.WriteLine $"{prefix} {production.Key} (q:TokenCursor) ="
        build writer production.Value 0 "q"
        writer.WriteLine ()
        prefix <- "and"

    writer.Flush ()

    Console.OutputEncoding <- Encoding.Unicode
    let src = File.ReadAllText("core.af")
    let cursor = Lexer.makeCursor src
    let tokens = Lexer.tokenise cursor |> Seq.toArray
    let tc = { Lexer.TokenCursor.source = tokens; Lexer.TokenCursor.index = 0 }
    //let result = Core.expr tc
    0

