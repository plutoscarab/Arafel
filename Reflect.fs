module Reflect

open System
open Microsoft.FSharp.Reflection

open Parse

let getImmediateDependencies (t:Type) =
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

let rec decodeParser ps =
    match ps with
    | "_"::rest ->
        ProductionP, rest
    | "opt"::rest ->
        let (p, rest') = decodeParser rest
        (OptionP p), rest'
    | "opt[]"::rest ->
        let (p, rest') = decodeParser rest
        (OptionListP p), rest'
    | "0+"::rest ->
        let (p, rest') = decodeParser rest
        (ListP p, rest')
    | "1+"::rest ->
        let (p, rest') = decodeParser rest
        (NonEmptyListP p, rest')
    | "and"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        (AndP(p, q), rest'')
    | "or"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        (OrP(p, q), rest'')
    | "delim"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        (DelimitedP(p, q), rest'')
    | "surr"::rest ->
        let (p, rest') = decodeParser rest
        let (q, rest'') = decodeParser rest'
        let (r, rest''') = decodeParser rest''
        (SurroundP(p, q, r), rest''')
    | s::rest when s.Length > 2 && s.StartsWith("'") && s.EndsWith("'") ->
        LiteralP(s.Substring(1, s.Length - 2)), rest
    | s::rest when s = s.ToUpperInvariant() ->
        TokenP(s), rest
    | _ ->
        raise (NotImplementedException())

let decodeParserStr (s:string) =
    let r, n = decodeParser (s.Split(' ') |> List.ofArray)
    if n <> [] then raise (Exception "")
    r

let primaryType (t:Type) =
    if t.Name = "String" then
        StringType
    elif t.Name = "BigInteger" then
        BigintType
    else
        ProductionType t.Name

let tupleField (t:Type) parser =
    if t.Name = "FSharpList`1" then
        TupleField(primaryType (t.GetGenericArguments().[0]), ListM, parser)
    elif t.Name = "FSharpOption`1" then
        TupleField(primaryType (t.GetGenericArguments().[0]), OptionM, parser)
    else
        TupleField(primaryType t, SingleM, parser)

let tupleFields (c:UnionCaseInfo) =
    let fields = c.GetFields()
    let n = Array.length fields

    let parsers =
        c.GetCustomAttributes()
        |> Array.filter (fun f -> f :? Parse.ParseAttribute)
        |> Array.map (fun f -> f :?> Parse.ParseAttribute)
        |> Array.map (fun f -> decodeParserStr f.Syntax)

    if n <> Array.length(parsers) then raise (Exception "")
    Array.zip fields parsers
    |> Array.map (fun (f, p) -> tupleField f.PropertyType p)
    |> List.ofArray

let unionCases (t:Type) =
    FSharpType.GetUnionCases(t)
    |> List.ofArray
    |> List.map (fun c ->
        UnionCase(c.Name, tupleFields c))

let rec getReferencedTypes pending result =
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
    |> List.map (fun t -> Production(t.Name, unionCases t))
