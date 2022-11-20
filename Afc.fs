
open System
open System.IO

open Arafel
open Cursor
open Lexer
open Localisation
open Parse

let rec orLast =
    function
    | [] -> []
    | [s] -> [s]
    | [p; u] -> [p; "or " + u]
    | h::t -> h::(orLast t)

let errStr e t =
    let ex = String.concat ", " (orLast e)

    match t with
    | [] ->
        loc.EndOfText(ex)
    | he::_ ->
        let cu = Tokens.tokenCursor he
        let tx = Tokens.tokenText he
        loc.LinePosToken(cu.line, cu.pos, tx, ex)

let parseExpressions src =
    let tokens = tokenise Arafel.keywords (mkCursor src) |> Seq.toList
    let mutable current = tokens

    seq {
        while current <> [] do
            let (result, next) = Arafel.expr current
            
            match result with
            | Nomatch e ->
                printfn "%s" (errStr e current)
            | SyntaxError e ->
                printfn "%s" (errStr e next)
            | Match expr ->
                yield expr

            if current <> next then
                current <- next
            else
                current <- List.tail current
    }
    |> Seq.toList

[<EntryPoint>]
let main (args: string[]) =
    let src = File.ReadAllText (args.[0])
    let exprs = parseExpressions src

    0