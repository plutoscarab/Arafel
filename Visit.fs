module Visit

open Arafel.CodeDom

let writeVisitorFile filename modulename productions =
    use file = System.IO.File.CreateText(filename)
    use writer = new System.CodeDom.Compiler.IndentedTextWriter(file)
    writer.WriteLine $"module {modulename}"
    writer.WriteLine ()
    writer.WriteLine "open System"
    writer.WriteLine "open Syntax"
    
    writer.WriteLine ()
    writer.WriteLine "type Visitor() ="
    writer.Indent <- writer.Indent + 1

    for Production (pname, cases, _) in productions do
        for UnionCase (cname, fields) in cases do
            let mname = pname + "_" + cname
            writer.WriteLine ()
            writer.Write $"abstract member {mname}: "
            let mutable fnames : string list = []
            let mutable oper = ""
            for TupleField (fname, primaryType, multiplicity, _) in fields do
                fnames <- fname::fnames
                writer.Write oper
                oper <- " * "
                match primaryType with
                | StringType -> writer.Write "string"
                | BigintType -> writer.Write "bigint"
                | ProductionType p -> writer.Write p
                match multiplicity with
                | SingleM -> ignore ()
                | OptionM -> writer.Write " option"
                | ListM -> writer.Write " list"
            fnames <- List.rev fnames
            writer.WriteLine $" -> {pname}"
            let nf = List.length fields
            let fi = [0 .. nf - 1]
            let ctor = fnames |> String.concat ", "
            let gtor = fnames |> List.map (fun s -> s + "'") |> String.concat ", "
            writer.WriteLine $"default this.{mname}({ctor}) ="
            writer.Indent <- writer.Indent + 1

            for TupleField (fname, primaryType, multiplicity, _), j in List.zip fields fi do
                match primaryType with
                | ProductionType p ->
                    writer.Write $"let {fname}' = "
                    match multiplicity with
                    | SingleM ->
                        writer.WriteLine $"this.Visit{p} {fname}"
                    | OptionM ->
                        writer.WriteLine $"Option.map this.Visit{p} {fname}"
                    | ListM ->
                        writer.WriteLine $"List.map this.Visit{p} {fname}"
                | _ ->
                    writer.WriteLine $"let {fname}' = {fname}"
            
            writer.WriteLine $"{cname} ({gtor})"
            writer.Indent <- writer.Indent - 1

        writer.WriteLine ()
        writer.WriteLine $"abstract member Visit{pname}: {pname} -> {pname}"
        writer.WriteLine $"default this.Visit{pname}(value) ="
        writer.Indent <- writer.Indent + 1
        writer.WriteLine "match value with"
        let nc = List.length cases
        let ci = [0 .. nc - 1]

        for UnionCase (cname, fields), i in List.zip cases ci do
            let ctor =
                fields
                |> List.map (fun (TupleField(name, _, _, _)) -> name)
                |> String.concat ", "
            writer.WriteLine $"| {cname} ({ctor}) -> this.{pname}_{cname}({ctor})"

        writer.Indent <- writer.Indent - 1

    writer.Indent <- writer.Indent - 1
