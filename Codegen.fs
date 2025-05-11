open System
open System.Diagnostics
open System.IO
open System.Text

open Syntax
open Reflect
open Ebnf
open Parse
open Print
open Random
open Visit

let private coreLocale = "en-us"
let private coreFilename = "res\\" + coreLocale + ".txt"

let union map altmap =
    let mutable m = altmap
    for (key, value) in Map.toSeq map do
        m <- Map.add key value m
    m

let rec readResources (filename: string) =
    File.ReadLines filename
    |> Seq.skip 1
    |> Seq.map (fun s ->
        let sp = s.IndexOf ' '
        (s.Substring(0, sp), s.Substring(sp).Trim()))
    |> Map

let writeResources (filename: string) =
    let locale = Path.GetFileNameWithoutExtension(filename)
    use w = File.CreateText $"generated/{locale}.fs"
    let lm = locale.Replace("-", "_")
    w.WriteLine $"module Localisation_{lm}"
    w.WriteLine "// Generated code. Do not edit."

    if filename = coreFilename then
        w.WriteLine ()
        w.WriteLine "open System"

    w.WriteLine ()
    w.WriteLine "type Strings() ="
    let baseLocale = Seq.head (File.ReadLines filename)

    if not (String.IsNullOrEmpty(baseLocale)) then
        let blm = baseLocale.Replace("-", "_")
        w.WriteLine $"    inherit Localisation_{blm}.Strings()"

    let res = readResources filename
    let core = readResources coreFilename

    for (id, format) in Map.toSeq res do
        let mutable f = Map.find id core
        let mutable i = 0
        let mutable parms = []

        while i <> -1 && i < String.length(f) do
            i <- f.IndexOf('{', i)
            if i <> -1 then
                let j = f.IndexOf('}', i)
                let pname = f.Substring(i + 1, j - i - 1)
                i <- j
                let dot = pname.IndexOf('.')
                let oname = if dot = -1 then pname else pname.Substring(0, dot)
                if not (List.contains oname parms) then
                    parms <- parms @ [oname]

        let pstr = String.concat ", " parms

        let tstr = 
            if List.isEmpty parms
                then "unit"
                else String.concat " * " (List.map (fun s -> "Object") parms)

        f <- format
        i <- 0

        while i <> -1 && i < String.length(f) do
            i <- f.IndexOf('{', i)
            if i <> -1 then
                let j = f.IndexOf('}', i)
                f <- f.Substring(0, i) + "«" + f.Substring(i, j - i + 1) + "»" + f.Substring(j + 1)
                i <- j + 2

        w.WriteLine ()

        if filename = coreFilename then
            w.WriteLine $"    abstract member {id} : {tstr} -> string"
            w.WriteLine $"    default _.{id}({pstr}) ="
        else
            w.WriteLine $"    override _.{id}({pstr}) ="

        w.WriteLine $"        $\"{f}\""

let writeLocalisation() =
    use locFile = File.CreateText "generated/localisation.fs"
    locFile.WriteLine "module Localisation"
    locFile.WriteLine "// Generated code. Do not edit."
    locFile.WriteLine ()
    locFile.WriteLine "let locMap = Map ["

    for file in Directory.GetFiles("res") do
        writeResources file
        let locale = Path.GetFileNameWithoutExtension(file)
        let lid = locale.Replace("-", "_")
        locFile.WriteLine $"    (\"{locale}\", Localisation_{lid}.Strings());"

    locFile.WriteLine "]"
    locFile.WriteLine ()
    locFile.WriteLine $"let loc = Map.find \"{coreLocale}\" locMap"

let writeUnicodeChart filename (runes: Rune seq) =
    use file = File.CreateText filename
    let rows = runes |> Seq.groupBy (fun r -> r.Value / 16)
    let mutable nrows = 0

    for row in rows do
        if nrows = 0 then
            file.WriteLine "|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | A | B | C | D | E | F |"
            file.WriteLine "|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|"
        let u = (fst row) * 16
        file.Write $"| U+{u:X4} |"
        for i in [0..15] do
            let ch = Rune(u + i)
            if Seq.contains ch (snd row) then
                file.Write $" {ch} |"
            else
                file.Write $" |"
        file.WriteLine ()
        nrows <- nrows + 1

        if nrows = 30 then
            nrows <- 0
            file.WriteLine ()
            file.WriteLine ()

let executeCommand executable args =
    let startInfo = ProcessStartInfo()
    startInfo.FileName <- executable
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true

    for a in args do
      startInfo.ArgumentList.Add(a)

    use p = new Process()
    p.StartInfo <- startInfo
    p.Start() |> ignore
    let output = new Text.StringBuilder()
    let error = new Text.StringBuilder()
    p.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
    p.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
    p.BeginErrorReadLine()
    p.BeginOutputReadLine()
    p.WaitForExit()

let writeRailroadDiagram filename grammar =
    executeCommand "cmd.exe" ["/C"; "java"; "-jar"; "rr.war"; "-out:" + filename; grammar]

let loc dir pattern =
        Directory.GetFiles(dir, pattern)
        |> Seq.map (File.ReadLines >> Seq.length)
        |> Seq.sum

let main =
    let productions = getProductions typeof<Expr>
    let keywords = getProductionKeywords productions
    writeEbnf "generated/grammar.txt" productions
    writeDot "generated/productions.dot" productions
    writeParserFile "generated/arafel.fs" "Parser" productions keywords
    writePrintFile "generated/pretty.fs" "Pretty" productions
    writeTesterFile "generated/tester.fs" "Tester" productions
    writeVisitorFile "generated/visitor.fs" "Visitor" productions
    writeUnicodeChart "generated/opChars.md" Lexer.opChars
    writeUnicodeChart "generated/idNonLetterChars.md" (Lexer.idContinuation |> Seq.filter (fun r -> not (Rune.IsLetter(r))))
    writeRailroadDiagram "generated/railroad.html" "generated/grammar.txt"
    writeLocalisation()

    let fs = loc "." "*.fs"
    let gen = loc "generated" "*.*"

    File.WriteAllText ("generated/loc.txt",
        (sprintf "Lines of code\nF#    %d\ngen'd %d\n" fs gen))

    0