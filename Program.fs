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

let main =

    Console.OutputEncoding <- Encoding.UTF8

    let productions = getProductions typeof<Statement>
    writeEbnf "generated/grammar.txt" productions
    writeParserFile "generated/arafel.fs" "Arafel" productions
    writePrintFile "generated/pretty.fs" "Pretty" productions

    let src = File.ReadAllText("sample.af", Encoding.UTF8)
    let runes = src.EnumerateRunes() |> Seq.toList
    let cursors = Cursor.getCursors runes |> Seq.toArray
    let cursor = Cursor.makeCursor src
    let tokens = Lexer.tokenise cursor |> Seq.toList
    use file = File.CreateText("generated/pretty.af")
    use writer = new IndentedTextWriter(file)

    let mutable t = tokens

    while t <> [] do

        let (r, t2) = Arafel.statement t

        let rec orLast =
            function
            | [] -> []
            | [s] -> [s]
            | [p; u] -> [p; "or " + u]
            | h::t -> h::(orLast t)

        let err e t =
            let ex = String.concat ", " (orLast e)
            let he = List.head t
            let cu = Tokens.tokenCursor he
            let tx = Tokens.tokenText he
            raise (Exception $"Line {cu.line} Pos {cu.pos} «{tx}»: Expected {ex}.")

        match r with
        | Nomatch e -> err e t
        | SyntaxError e -> err e t2
        | Match e ->
            Pretty.printStatement writer e
            writer.WriteLine ()

        t <- t2

    0