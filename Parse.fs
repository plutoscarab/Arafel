module Parse

open Lexer

type ParseTree = Token of Token | Node of ParseTree list | Empty | Production of string * ParseTree

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