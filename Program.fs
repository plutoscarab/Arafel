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

let main =
    let grammar =
        File.ReadAllLines "core.grammar.txt"
        |> Seq.map Ebnf.parseProduction
        |> Map.ofSeq

    let writer = new IndentedTextWriter (File.CreateText "Core.fs")
    writer.WriteLine "module Core"
    writer.WriteLine ()
    writer.WriteLine "open Lexer"
    writer.WriteLine ()
    writer.WriteLine "type ParseTree = Token of Token | Node of ParseTree list | Empty"
    writer.WriteLine ""
    writer.WriteLine "type Result = NoMatch | Ok of ParseTree * TokenCursor"
    writer.WriteLine ""
    writer.WriteLine "let rec c choices (t:TokenCursor) ="
    writer.WriteLine "    match choices with"
    writer.WriteLine "    | [] -> NoMatch"
    writer.WriteLine "    | p::rest ->"
    writer.WriteLine "        match p t with"
    writer.WriteLine "        | NoMatch -> c rest t"
    writer.WriteLine "        | Ok (result, next) -> Ok (result, next)"
    writer.WriteLine ()
    writer.WriteLine "let s items (t:TokenCursor) ="
    writer.WriteLine "    let rec s' items list (t:TokenCursor) ="
    writer.WriteLine "        match items with"
    writer.WriteLine "        | [] -> Ok (Node (List.rev list), t)"
    writer.WriteLine "        | p::rest ->"
    writer.WriteLine "            match p t with"
    writer.WriteLine "            | NoMatch -> NoMatch"
    writer.WriteLine "            | Ok (result, next) -> s' rest (result::list) next"
    writer.WriteLine "    s' items [] t"
    writer.WriteLine ()
    writer.WriteLine "let t a (t:TokenCursor) = NoMatch"
    writer.WriteLine ()
    writer.WriteLine "let l str (t:TokenCursor) ="
    writer.WriteLine "    if str = t.Str then"
    writer.WriteLine "        Ok (Token t.Current, t.Next)"
    writer.WriteLine "    else"
    writer.WriteLine "        NoMatch"
    writer.WriteLine ()
    writer.WriteLine "let z parser (t:TokenCursor) ="
    writer.WriteLine ""
    writer.WriteLine "    let rec z' list (t:TokenCursor) ="
    writer.WriteLine "        match parser t with"
    writer.WriteLine "        | NoMatch -> Ok (Node (List.rev list), t)"
    writer.WriteLine "        | Ok (result, next) -> z' (result::list) next"
    writer.WriteLine ()
    writer.WriteLine "    z' [] t"
    writer.WriteLine ()
    writer.WriteLine "let m parser (t:TokenCursor) ="
    writer.WriteLine "    match z parser t with"
    writer.WriteLine "    | NoMatch -> NoMatch"
    writer.WriteLine "    | Ok (Node [], _) -> NoMatch"
    writer.WriteLine "    | Ok (result, next) -> Ok (result, next)"
    writer.WriteLine ()
    writer.WriteLine "let o parser (t:TokenCursor) ="
    writer.WriteLine "    match parser t with"
    writer.WriteLine "    | NoMatch -> Ok (Empty, t)"
    writer.WriteLine "    | Ok (result, next) -> Ok (result, next)"
    writer.WriteLine ()
    let mutable prefix = "let rec"

    for production in grammar do
        writer.WriteLine $"{prefix} {production.Key} q ="
        writer.WriteLine $"    {exprStr production.Value} q"
        writer.WriteLine ()
        prefix <- "and"

    writer.Flush ()

    let src = File.ReadAllText("core.af")
    let cursor = Lexer.makeCursor src
    Console.OutputEncoding <- Encoding.Unicode
    let mutable c = cursor

    while c.More do
        if c.pos = 1 then
            printf "Line %d: " (c.line)
        printf "%s" (c.Str)
        c <- c.Next

    printfn ""

    for token in Lexer.tokenise cursor do
        printf "%s, " (Lexer.tokenStr token)
    
    let tokens = Lexer.tokenise cursor |> Seq.toArray
    let tc = { Lexer.TokenCursor.source = tokens; Lexer.TokenCursor.index = 0 }
    let result = Core.expr tc
    0

