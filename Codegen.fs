open System
open System.IO

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
        (Int32.Parse(s.Substring(0, 8).Trim()), s.Substring(8)))
    |> Map

let writeResources (filename: string) =
    let locale = Path.GetFileNameWithoutExtension(filename)
    use w = File.CreateText $"generated/{locale}.fs"
    let lm = locale.Replace("-", "_")
    w.WriteLine $"module Localisation_{lm}"

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

        let sanitise s =
            s
            |> Seq.filter (Char.IsLetter)
            |> String.Concat

        let normcase (s: string) =
            s.Substring(0, 1).ToUpperInvariant() + s.Substring(1).ToLowerInvariant()

        let words =
            (Map.find id core).Split(' ')
            |> Seq.filter (fun w -> not (w.StartsWith("{")))
            |> Seq.map sanitise
            |> Seq.filter (fun s -> not (String.IsNullOrEmpty(s)))
            |> Seq.take 3
            |> Seq.map normcase
            |> String.concat ""

        w.WriteLine ()
        w.WriteLine $"    // ID {id}"

        if filename = coreFilename then
            w.WriteLine $"    abstract member {words} : {tstr} -> string"
            w.WriteLine $"    default _.{words}({pstr}) ="
        else
            w.WriteLine $"    override _.{words}({pstr}) ="

        w.WriteLine $"        $\"{f}\""

let writeLocalisation() =
    use locFile = File.CreateText "generated/localisation.fs"
    locFile.WriteLine "module Localisation"
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

let loc dir pattern =
        Directory.GetFiles(dir, pattern)
        |> Seq.map (File.ReadLines >> Seq.length)
        |> Seq.sum

let main =
    let productions = getProductions typeof<Command>
    let keywords = getProductionKeywords productions
    writeEbnf "generated/grammar.txt" productions
    writeParserFile "generated/arafel.fs" "Arafel" productions keywords
    writePrintFile "generated/pretty.fs" "Pretty" productions
    writeTesterFile "generated/tester.fs" "Tester" productions
    writeVisitorFile "generated/visitor.fs" "Visitor" productions
    writeLocalisation()

    let fs = loc "." "*.fs"
    let gen = loc "generated" "*.*"

    File.WriteAllText ("generated/loc.txt",
        (sprintf "F#    %d\ngen'd %d\n" fs gen))

    0