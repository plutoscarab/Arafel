module Lexer

open System
open System.Globalization
open System.Text

open Cursor
open Tokens

let private isWhitespace r =
    " \t\r\n".Contains(r.ToString()[0])

let rec private emptyLine (cursor:Cursor) =
    if cursor.More then
        match cursor.Str with
        | "\r" | "\n" -> true
        | " " | "\t" -> emptyLine cursor.Next
        | _ -> false
    else
        true

let superchars = "⁰¹²³⁴⁵⁶⁷⁸⁹"

let idContinuation =
    [0..0xD7FF] @ [0xE000..0x10FFFF]
    |> Seq.map Rune
    |> Seq.filter (fun r ->
        let s = r.ToString()
        s = "_"
        || Rune.IsLetter(r)
        || Rune.IsDigit(r)
        || (Rune.GetUnicodeCategory(r) = UnicodeCategory.OtherNumber && not (superchars.Contains(s)))
        || "'†‡′″‴⁗".Contains(s))
    |> Set.ofSeq

let opChars =
    [0..0xD7FF] @ [0xE000..0x10FFFF]
    |> Seq.map Rune
    |> Seq.filter (fun r ->
        let u = Rune.GetUnicodeCategory r
        u = UnicodeCategory.OtherPunctuation 
        || u = UnicodeCategory.MathSymbol 
        || u = UnicodeCategory.OtherSymbol 
        || u = UnicodeCategory.DashPunctuation)
    |> Set.ofSeq

let tokenise (keywords:Set<string>) (cursor:Cursor) =
    seq {
        let mutable c = cursor
        let mutable line = 0

        while c.More do

            while c.More && line <> c.line && c.pos = 1 && (emptyLine c || not (isWhitespace (c.Current))) do
                line <- c.line
                while c.More && line = c.line do
                    c <- c.Next

            let uc = Rune.GetUnicodeCategory(c.Current)

            match c.Current with
            | r when isWhitespace r ->
                let cl = c.line
                while isWhitespace(c.Current) && cl = c.line do
                    c <- c.Next
            | r when c.Str = "," ->
                yield Punctuation (c, c.Next)
                c <- c.Next
            | r when Rune.IsLetter r || c.Str = "_" ->
                let mutable start = c
                while Set.contains (c.Current) idContinuation do
                    c <- c.Next
                let s = spanned (start, c)
                if s.Length = 0 then
                    raise (Exception "Lexer bug: Identifier start set not subset of continuation set")
                if s = "false" || s = "𝗙" then
                    yield Bool (start, c)
                elif s = "true" || s = "𝗧" then
                    yield Bool (start, c)
                elif Set.contains s keywords then
                    yield Keyword (start, c)
                else
                    yield Identifier (start, c)
            | r when Rune.IsDigit r ->
                let mutable start = c
                while Rune.IsDigit (c.Current) || c.Str = "_" do
                    c <- c.Next
                yield Nat (start, c)
            | r when superchars.Contains(r.ToString()) ->
                let mutable start = c
                while superchars.Contains(c.Str) do
                    c <- c.Next
                yield Superscript (start, c)
            | r when r.ToString() = "\"" ->
                c <- c.Next
                let mutable start = c
                while c.More && c.Str <> "\"" do
                    c <- c.Next
                if c.More then
                    yield String (start, c)
                    c <- c.Next
                else
                    yield Error c
            | r when Set.contains r opChars ->
                let mutable start = c
                while Set.contains (c.Current) opChars do
                    c <- c.Next
                yield Operator (start, c)
            | r when uc = UnicodeCategory.OpenPunctuation || uc = UnicodeCategory.ClosePunctuation ->
                yield Punctuation (c, c.Next)
                c <- c.Next
            | _ ->
                if c.More then
                    yield Error c
                    c <- c.Next

    } |> Array.ofSeq
