module Lexer

open System.Globalization
open System.Text

open Cursor
open Tokens

let isWhitespace r =
    " \t\r\n".Contains(r.ToString()[0])

let tokenise (cursor:Cursor) =
    seq {
        let mutable c = cursor
        let mutable line = 0

        while c.More do
            let commentStart = c

            while c.More && line <> c.line && c.pos = 1 && (c.Str = "\r" || c.Str = "\n" || not (isWhitespace (c.Current))) do
                line <- c.line
                while line = c.line do
                    c <- c.Next

            if c.index > commentStart.index then
                yield Comment (commentStart, c)

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
