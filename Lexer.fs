module Lexer

open System.Globalization
open System.Text

type Cursor =
    { source: Rune array;
      index: int;
      line: int;
      pos: int }

    member c.More = c.index < c.source.Length

    member c.Current = if c.More then c.source[c.index] else new Rune(0)

    member c.Str = c.Current.ToString()

    member c.Next = 
        if c.More then 
            match c.Str with
            | "\n" | "\r" when c.index > c.source.Length - 2 || c.source[c.index + 1].ToString() <> "\n" -> 
                { c with index = c.index + 1; line = c.line + 1; pos = 1 }
            | _ ->
                { c with index = c.index + 1; pos = c.pos + 1 }
        else c

type Span = Cursor * Cursor

let spanned (start:Cursor, next:Cursor) =
    [start.index .. next.index - 1] |> List.map (fun i -> start.source[i].ToString()) |> String.concat ""

let makeCursor (source:string) =
    let runes = source.EnumerateRunes() |> Array.ofSeq
    { source = runes; index = 0; line = 1; pos = 1 }

type Token = 
    | Id of Span
    | Operator of Span
    | Punctuation of Span
    | Nat of Span
    | String of Span
    | Error of Cursor
    | EndOfText

let tokenStr (token:Token) =
    match token with
    | Id s -> "Id " + spanned s
    | Operator s -> "Operator " + spanned s
    | Punctuation s -> "Punctuation " + spanned s
    | Nat s -> "Nat " + spanned s
    | String s -> "String " + spanned s
    | Error c -> "Error " + (c.Str)
    | EndOfText -> ""

let tokenText (token:Token) =
    match token with
    | Id s -> spanned s
    | Operator s -> spanned s
    | Punctuation s -> spanned s
    | Nat s -> spanned s
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
            if line <> c.line && c.pos = 1 && not (isWhitespace (c.Current)) then
                line <- c.line
                while line = c.line do
                    c <- c.Next
                line <- c.line
            else
                let uc = Rune.GetUnicodeCategory (c.Current)
                match c.Current with
                | r when isWhitespace r -> 
                    while isWhitespace (c.Current) do
                        c <- c.Next
                | r when Rune.IsLetter r || c.Str = "_" ->
                    let mutable start = c
                    while Rune.IsLetter (c.Current) || Rune.IsDigit (c.Current) || c.Str = "_" do
                        c <- c.Next
                    yield Id (start, c)
                | r when Rune.IsDigit r ->
                    let mutable start = c
                    while Rune.IsDigit (c.Current) do
                        c <- c.Next
                    yield Nat (start, c)
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
