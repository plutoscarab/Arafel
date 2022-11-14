module Lexer

open System.Globalization
open System.Text

open Cursor

type Token = 
    | Keyword of Cspan
    | Id of Cspan
    | Operator of Cspan
    | Punctuation of Cspan
    | Nat of Cspan
    | Superscript of Cspan
    | String of Cspan
    | Error of Cursor
    | EndOfText

let tokenStr (token:Token) =
    match token with
    | Keyword s -> "Keyword " + spanned s
    | Id s -> "Id " + spanned s
    | Operator s -> "Operator " + spanned s
    | Punctuation s -> "Punctuation " + spanned s
    | Nat s -> "Nat " + spanned s
    | Superscript s -> "Superscript " + spanned s
    | String s -> "String " + spanned s
    | Error c -> "Error " + (c.Str)
    | EndOfText -> ""

let tokenText (token:Token) =
    match token with
    | Keyword s -> spanned s
    | Id s -> spanned s
    | Operator s -> spanned s
    | Punctuation s -> spanned s
    | Nat s -> spanned s
    | Superscript s -> spanned s
    | String s -> spanned s
    | Error c -> c.Str
    | EndOfText -> ""

type TokenCursor =
    { source: Token array;
      index: int }

    member c.More = c.index < c.source.Length

    member c.Current = if c.More then c.source[c.index] else EndOfText

    member c.Str = tokenStr (c.Current)

    member c.Next = 
        if c.More then 
            { c with index = c.index + 1 }
        else 
            c

let isWhitespace r =
    " \t\r\n".Contains(r.ToString()[0])

let tokenise (cursor:Cursor) =
    seq {
        let mutable c = cursor
        let mutable line = 0

        while c.More do
            if line <> c.line && c.pos = 1 && (c.Str = "\r" || c.Str = "\n" || not (isWhitespace (c.Current))) then
                line <- c.line
                while line = c.line do
                    c <- c.Next
            else
                let uc = Rune.GetUnicodeCategory(c.Current)
                match c.Current with
                | r when isWhitespace r -> 
                    while isWhitespace (c.Current) do
                        c <- c.Next
                | r when Rune.IsLetter r || c.Str = "_" ->
                    let mutable start = c
                    while Rune.IsLetter (c.Current) || Rune.IsDigit (c.Current) || c.Str = "_" do
                        c <- c.Next
                    let s = spanned (start, c)
                    if s = "let" || s = "case" || s = "type" || s = "forall" || s = "if" then
                        yield Keyword (start, c)
                    else
                        yield Id (start, c)
                | r when Rune.IsDigit r ->
                    let mutable start = c
                    let mutable previous = c
                    let mutable wasComma = false
                    while Rune.IsDigit (c.Current) || (c.Str = "_" && not wasComma) do
                        wasComma <- c.Str = "_"
                        previous <- c
                        c <- c.Next
                    if wasComma then c <- previous
                    yield Nat (start, c)
                | r when "⁰¹²³⁴⁵⁶⁷⁸⁹".Contains(r.ToString()[0]) ->
                    let mutable start = c
                    while "⁰¹²³⁴⁵⁶⁷⁸⁹".Contains(c.Str[0]) do
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
                | r when uc = UnicodeCategory.OtherPunctuation || uc = UnicodeCategory.MathSymbol || uc = UnicodeCategory.OtherSymbol || uc = UnicodeCategory.DashPunctuation ->
                    let mutable start = c
                    while Rune.GetUnicodeCategory (c.Current) = UnicodeCategory.OtherPunctuation || Rune.GetUnicodeCategory (c.Current) = UnicodeCategory.MathSymbol || Rune.GetUnicodeCategory (c.Current) = UnicodeCategory.OtherSymbol || Rune.GetUnicodeCategory(c.Current) = UnicodeCategory.DashPunctuation do
                        c <- c.Next
                    yield Operator (start, c)
                | r when uc = UnicodeCategory.OpenPunctuation || uc = UnicodeCategory.ClosePunctuation ->
                    yield Punctuation (c, c.Next)
                    c <- c.Next
                | _ ->
                    yield Error c
                    c <- c.Next
    } |> Array.ofSeq
