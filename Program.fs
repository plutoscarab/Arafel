open System
open System.Globalization
open System.IO
open System.Linq
open System.Numerics
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

let makeCursor (source:string) =
    let runes = source.EnumerateRunes() |> Array.ofSeq
    { source = runes; index = 0; line = 1; pos = 1 }

type Span = Cursor * Cursor

let spanStr (s:Span) =
    let source = (fst s).source
    [(fst s).index .. ((snd s).index - 1)] |> Seq.map (fun index -> source[index]) |> Seq.map (fun rune -> rune.ToString()) |> String.concat ""

type Token = 
    | Identifier of Span
    | Operator of Span
    | Nat of Span
    | String of Span
    | Error of Cursor

let tokenStr (token:Token) =
    match token with
    | Identifier span -> "Identifier " + (spanStr span)
    | Operator span -> "Operator " + (spanStr span)
    | Nat span -> "Nat " + (spanStr span)
    | String span -> "String " + (spanStr span)
    | Error c -> "Error " + (c.Str)

let isWhitespace r =
    let uc = Rune.GetUnicodeCategory r
    uc = UnicodeCategory.SpaceSeparator || uc = UnicodeCategory.LineSeparator

let tokenise (cursor:Cursor) =
    seq {
        let mutable c = cursor
        let mutable line = 0

        while c.More do
            if line <> c.line && not (isWhitespace (c.Current)) then
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
                    yield Identifier (start, c)
                | r when Rune.IsDigit r ->
                    let mutable start = c
                    while Rune.IsDigit (c.Current) do
                        c <- c.Next
                    yield Nat (start, c)
                | r when uc = UnicodeCategory.MathSymbol || uc = UnicodeCategory.OtherSymbol ->
                    let mutable start = c
                    while Rune.GetUnicodeCategory (c.Current) = UnicodeCategory.MathSymbol || Rune.GetUnicodeCategory (c.Current) = UnicodeCategory.OtherSymbol do
                        c <- c.Next
                    yield Operator (start, c)
                | _ ->
                    yield Error c
                    c <- c.Next
    } |> Array.ofSeq

let main =
    for line in File.ReadAllLines "core.grammar.txt" do
        let pair = line.Split("::=")
        printf "%s ::= " (pair[0].Trim())
        let expr = Ebnf.parseExpr (pair[1].Trim())
        printfn "%s" (Ebnf.show expr)

    let src = File.ReadAllText("core.af")
    let cursor = makeCursor src
    Console.OutputEncoding <- Encoding.Unicode
    let mutable c = cursor

    while c.More do
        if c.pos = 1 then
            printf "Line %d: " (c.line)
        printf "%s" (c.Str)
        c <- c.Next

    printfn ""

    for token in tokenise cursor do
        printf "%s, " (tokenStr token)
    
    0

