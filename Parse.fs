module Parse

open Lexer

type ParseTree = Token of Token | Node of ParseTree list | Empty

type Result = NoMatch | Ok of ParseTree * TokenCursor

let rec c choices (t:TokenCursor) =
    match choices with
    | [] -> NoMatch
    | p::rest ->
        match p t with
        | NoMatch -> c rest t
        | Ok (result, next) -> Ok (result, next)

let s items (t:TokenCursor) =
    let rec s' items list (t:TokenCursor) =
        match items with
        | [] -> Ok (Node (List.rev list), t)
        | p::rest ->
            match p t with
            | NoMatch -> NoMatch
            | Ok (result, next) -> s' rest (result::list) next
    s' items [] t

let t (a:obj) (t:TokenCursor) =
    match a with
    | :? (string -> Token) as f ->
        match t.Current with
        | Id x -> if t.Current = f x then Ok (Token t.Current, t.Next) else NoMatch
        | String x -> if t.Current = f x then Ok (Token t.Current, t.Next) else NoMatch
        | Operator x -> if t.Current = f x then Ok (Token t.Current, t.Next) else NoMatch
        | _ -> NoMatch
    | :? (bigint -> Token) as f ->
        match t.Current with
        | Nat x -> if t.Current = f x then Ok (Token t.Current, t.Next) else NoMatch
        | _ -> NoMatch
    | _ -> NoMatch

let l str (t:TokenCursor) =
    if t.Current = Id str then
        Ok (Token t.Current, t.Next)
    else
        NoMatch

let z parser (t:TokenCursor) =

    let rec z' list (t:TokenCursor) =
        match parser t with
        | NoMatch -> Ok (Node (List.rev list), t)
        | Ok (result, next) -> z' (result::list) next

    z' [] t

let m parser (t:TokenCursor) =
    match z parser t with
    | NoMatch -> NoMatch
    | Ok (Node [], _) -> NoMatch
    | Ok (result, next) -> Ok (result, next)

let o parser (t:TokenCursor) =
    match parser t with
    | NoMatch -> Ok (Empty, t)
    | Ok (result, next) -> Ok (result, next)