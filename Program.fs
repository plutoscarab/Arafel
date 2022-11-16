open System
open System.CodeDom.Compiler
open System.Globalization
open System.IO
open System.Linq
open System.Numerics
open System.Text

open Language
open Reflect
open Parse
open Print
open Ebnf

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

        match r with
        | Nomatch e ->
            let ex = String.concat ", " e
            let c = Tokens.tokenCursor (List.head t)
            raise (Exception $"Line {c.line} Pos {c.pos} Expected {ex}")
        | SyntaxError e ->
            let ex = String.concat ", " e
            let c = Tokens.tokenCursor (List.head t)
            raise (Exception $"Line {c.line} Pos {c.pos} Expected {ex}")
        | Match e ->
            Pretty.printStatement writer e
            writer.WriteLine ()

        t <- t2

    0