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

    Console.OutputEncoding <- Encoding.Unicode
    (*
    let languageGrammar = buildTypes "language.txt" "Language.fs" "Language"
    let coreGrammar = readGrammar "core.grammar.txt"
    buildGrammar coreGrammar "Core.fs" "Core"
    let grammar = readGrammar "grammar.txt"
    buildGrammar grammar "Arafel.fs" "Arafel"

    let src = File.ReadAllText("sample.af")
    let cursor = Lexer.makeCursor src
    let tokens = Lexer.tokenise cursor |> Seq.toArray
    let mutable tc = { Lexer.TokenCursor.source = tokens; Lexer.TokenCursor.index = 0 }

    while tc.More do
        let result = Arafel.expr tc
        match result with
        | (Parse.Error, _) -> raise (Exception $"failed at token {tc.index}")
        | (tree, next) ->
            Console.WriteLine ()
            dump tree "" true
            let syntax = syntaxTree tree
            tc <- next
    *)

    let productions = getProductions typeof<Statement>
    writeEbnf "language.txt" productions
    writeParserFile "Arafel.fs" "Arafel" productions
    writePrintFile "Pretty.fs" "Pretty" productions

    let src = File.ReadAllText("sample.af")
    let runes = src.EnumerateRunes() |> Seq.toList
    let cursors = Cursor.getCursors runes |> Seq.toArray
    let cursor = Cursor.makeCursor src
    let tokens = Lexer.tokenise cursor |> Seq.toList
    use file = File.CreateText("sample.pretty.af")
    use writer = new IndentedTextWriter(file)

    let mutable t = tokens

    while t <> [] do
        let (r, t2) = Arafel.statement t

        match r with
        | None ->
            raise (Exception "")
        | Some e ->
            Pretty.printStatement writer e

        t <- t2

    0