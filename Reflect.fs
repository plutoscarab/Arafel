module Reflect

open System
open Microsoft.FSharp.Reflection

open Arafel.CodeDom
open Print

let private getImmediateDependencies (t:Type) =
    let deps =
        if FSharpType.IsTuple(t) then
            FSharpType.GetTupleElements(t) 
            |> Seq.ofArray
        elif FSharpType.IsUnion(t) then
            FSharpType.GetUnionCases(t) 
            |> Seq.ofArray
            |> Seq.map (fun c -> c.GetFields() |> Seq.ofArray |> Seq.map (fun f -> f.PropertyType))
            |> Seq.concat
        elif t.IsGenericType then
            t.GetGenericArguments() 
            |> Seq.ofArray
        else
            Seq.empty
    deps 
    |> Seq.map (fun f ->
        if f.Name = "FSharpList`1" || f.Name = "FSharpOption`1" then
            f.GetGenericArguments().[0]
        else
            f)
    |> Seq.distinct
    |> List.ofSeq

let private primaryType (t:Type) =
    if t.Name = "String" then
        StringType
    elif t.Name = "BigInteger" then
        BigintType
    elif t.Name = "Boolean" then
        BoolType
    else
        ProductionType t.Name

let private tupleField name (t:Type) parser =
    if t.Name = "FSharpList`1" then
        TupleField(name, primaryType (t.GetGenericArguments().[0]), ListM, parser)
    elif t.Name = "FSharpOption`1" then
        TupleField(name, primaryType (t.GetGenericArguments().[0]), OptionM, parser)
    else
        TupleField(name, primaryType t, SingleM, parser)

let private tupleFields (c:UnionCaseInfo) =
    let fields = c.GetFields()
    let n = Array.length fields

    let parsers =
        c.GetCustomAttributes()
        |> Array.filter (fun f -> f :? Parse.ParseAttribute)
        |> Array.map (fun f -> f :?> Parse.ParseAttribute)
        |> Array.map (fun f -> Parse.decodeParserStr f.Syntax)

    if n <> Array.length(parsers) then raise (Exception "")
    Array.zip fields parsers
    |> Array.map (fun (f, p) -> tupleField f.Name f.PropertyType p)
    |> List.ofArray

let private unionCases (t:Type) =
    FSharpType.GetUnionCases(t)
    |> List.ofArray
    |> List.map (fun c ->
        UnionCase(c.Name, tupleFields c))

let private indentOf (t:Type) =
    match t.GetCustomAttributes(typeof<IndentAttribute>, true) with
    | [||] -> false
    | _ -> true

let rec private getReferencedTypes pending result =
    match pending with
    | [] -> result
    | t::rest ->
        if (List.contains t result) then
            getReferencedTypes rest result
        else
            getReferencedTypes ((getImmediateDependencies t) @ rest) (t::result)

let getProductions (t:Type) =
    let moduleName = t.FullName.Substring(0, t.FullName.IndexOf('+') + 1)
    getReferencedTypes [t] []
    |> List.filter (fun f -> f.FullName.StartsWith(moduleName))
    |> List.map (fun t -> Production(t.Name, unionCases t, indentOf t))
    |> List.sortBy (fun (Production(name, _, _)) -> name)

let private getKeywordsOfField (TupleField(_, _, _, parser)) =
    Parse.getKeywords parser

let private getKeywordsOfCase (UnionCase(_, fields)) =
    Seq.concat (Seq.map getKeywordsOfField fields)

let private getKeywordsOfProduction (Production(_, cases, _)) =
    Set(Seq.concat (Seq.map getKeywordsOfCase cases))

let rec getProductionKeywords =
    function
    | [] ->
        Set.empty
    | p::rest ->
        Set.union (getKeywordsOfProduction p) (getProductionKeywords rest)
