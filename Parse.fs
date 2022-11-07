module Parse

open Lexer

type ParseTree = 
    | Empty
    | Error
    | Token of Token
    | Node of ParseTree list
    | Production of string * ParseTree

type Result = NoMatch | Ok of ParseTree * TokenCursor

let parseTreeFromList list =
    let rec flatten accum list =
        match list with
        | [] -> accum
        | h::t -> 
            match h with
            | Node sublist -> flatten (accum @ sublist) t
            | _ -> flatten (accum @ [h]) t
    let trimmed = list |> flatten [] |> List.filter (fun tree -> tree <> Empty)
    match trimmed with
    | [] -> Empty
    | [t] -> t
    | _ -> Node trimmed

let isNat (q:TokenCursor) =
    match q.Current with
    | Nat x -> (Token q.Current, q.Next)
    | _ -> (Error, q)

let isString (q:TokenCursor) =
    match q.Current with
    | String x -> (Token q.Current, q.Next)
    | _ -> (Error, q)

let isId (q:TokenCursor) =
    match q.Current with
    | Id x -> (Token q.Current, q.Next)
    | _ -> (Error, q)

let isOperator (q:TokenCursor) =
    match q.Current with
    | Operator x -> (Token q.Current, q.Next)
    | _ -> (Error, q)

let isText (s:string) (q:TokenCursor) =
    if tokenText (q.Current) = s then 
        (Token q.Current, q.Next) 
    else
        (Error, q)
