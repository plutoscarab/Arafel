module Cursor

open System.Text

type Cursor =
    { line: int
      pos: int
      index: int
      source: Rune array
    }

    member c.More = c.index < c.source.Length

    member c.Current = if c.More then c.source[c.index] else new Rune(0)

    member c.Str = c.Current.ToString()

    member c.Next = 
        if c.More then 
            match c.Str with
            | "\n" ->
                { c with index = c.index + 1; line = c.line + 1; pos = 1 }
            | "\r" when c.index > c.source.Length - 2 || c.source[c.index + 1].ToString() <> "\n" -> 
                { c with index = c.index + 1; line = c.line + 1; pos = 1 }
            | _ ->
                { c with index = c.index + 1; pos = c.pos + 1 }
        else c

type Cspan = Cursor * Cursor

let spanned (span: Cspan) =
    let (start, next) = span

    [start.index .. next.index - 1]
    |> List.map (fun i -> start.source[i].ToString())
    |> String.concat ""

let makeCursor (source:string) =
    let runes = source.EnumerateRunes() |> Array.ofSeq
    { source = runes; index = 0; line = 1; pos = 1 }

let rec getCursors' c runes =
    seq {
        let c2 =
            match runes with
            | [] -> c
            | r::n::rs when r.ToString() = "\r" && n.ToString() <> "\n" ->
                {c with index = c.index + 1; line = c.line + 1; pos = 1}
            | r::rs when r.ToString() = "\n" || r.ToString() = "\r" ->
                {c with index = c.index + 1; line = c.line + 1; pos = 1}
            | r::rs ->
                {c with index = c.index + 1; pos = c.pos + 1 }
        match runes with
        | [] ->
            ignore ()
        | _::rs ->
            yield c
            yield! (getCursors' c2 rs)
    }

let getCursors runes =
    getCursors'
        { source = runes |> Array.ofList; index = 0; line = 1; pos = 1 }
        runes
