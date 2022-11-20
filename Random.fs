module Random

open System
open System.CodeDom.Compiler
open System.IO

open Arafel.CodeDom

let mkOption mk =
    fun (rand: Random) depth ->
        if rand.Next(2 + depth) = 0
            then Some (mk rand depth)
            else None

let rec mkList mk =
    fun (rand: Random) depth ->
        if rand.Next(5) > depth
            then (mk rand depth)::((mkList mk) rand depth)
            else []

let rec mkNonempty mk =
    fun (rand: Random) depth ->
        (mk rand depth)::((mkList mk) rand depth)

let mkChar (rand: Random) depth =
    char (rand.Next(26) + int 'a')

let mkString rand depth =
    (mkChar rand 0)::((mkList mkChar) rand 0) |> String.Concat

let mkDigit (rand: Random) depth =
    bigint (rand.Next(100))

let mkBigint rand depth =
    (mkList mkDigit) rand 3
    |> List.fold (fun x y -> (bigint 100) * x + y) (bigint 0)

let writeTesterFile filename modulename (productions:Production list) =
    use file = File.CreateText(filename)
    use writer = new IndentedTextWriter(file)
    writer.WriteLine $"module {modulename}"
    writer.WriteLine ()
    writer.WriteLine "open System"
    writer.WriteLine "open Random"
    writer.WriteLine "open Syntax"
    
    let mutable keyword = "let rec"

    for Production(name, cases, indent) in productions do
        writer.WriteLine ()
        writer.WriteLine $"{keyword} random{name} (rand: Random) depth ="
        keyword <- "and"
        writer.Indent <- writer.Indent + 1
        let nc = List.length cases
        writer.WriteLine $"match rand.Next({nc}) with"
        let ci = [0 .. nc - 1]

        for UnionCase(name, fields), i in List.zip cases ci do
            let si = if i = nc - 1 then "_" else $"{i}"
            writer.WriteLine $"| {si} ->"
            writer.Indent <- writer.Indent + 1
            let nf = List.length fields
            let fi = [0 .. nf - 1]
            let mutable fnames : string list = []

            for TupleField(fname, primaryType, multiplicity, parser), j in List.zip fields fi do
                writer.Write $"let {fname} = "
                fnames <- fnames @ [fname]

                match multiplicity with
                | SingleM -> ()
                | OptionM -> writer.Write $"mkOption "
                | ListM ->
                    if isNonempty parser
                        then writer.Write $"mkNonempty "
                        else writer.Write $"mkList "

                match primaryType with
                | StringType -> writer.Write "mkString"
                | BigintType -> writer.Write "mkBigint"
                | ProductionType p -> writer.Write $"random{p}"

                writer.WriteLine " rand (depth + 1)"
            
            let ctor = fnames |> String.concat ", "
            writer.WriteLine $"{name}({ctor})"
            writer.Indent <- writer.Indent - 1

        writer.Indent <- writer.Indent - 1
