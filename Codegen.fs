open System
open System.IO

open Syntax
open Reflect
open Ebnf
open Parse
open Print

let writeResources (locale: string) =
    use w = File.CreateText $"generated/{locale}.fs"
    let lm = locale.Replace("-", "_")
    w.WriteLine $"module Localisation_{lm}"
    w.WriteLine ()
    w.WriteLine "type Strings() ="

    let res =
        File.ReadAllLines $"res/{locale}.txt"
        |> Seq.map (fun s ->
            (Int32.Parse(s.Substring(0, 8).Trim()), s.Substring(8)))

    for (id, format) in res do
        let mutable f = format
        let mutable i = 0
        let mutable parms = []
        while i <> -1 && i < String.length(f) do
            i <- f.IndexOf('{', i)
            if i <> -1 then
                let j = f.IndexOf('}', i)
                let pname = f.Substring(i + 1, j - i - 1)
                f <- f.Substring(0, i) + "«" + f.Substring(i, j - i + 1) + "»" + f.Substring(j + 1)
                i <- j + 2
                let dot = pname.IndexOf('.')
                let oname = if dot = -1 then pname else pname.Substring(0, dot)
                if not (List.contains oname parms) then
                    parms <- parms @ [oname]
        let pstr = String.concat ", " parms

        let sanitise s =
            s
            |> Seq.filter (Char.IsLetter)
            |> String.Concat

        let normcase (s: string) =
            s.Substring(0, 1).ToUpperInvariant() + s.Substring(1).ToLowerInvariant()

        let words =
            f.Split(' ')
            |> Seq.filter (fun w -> not (w.StartsWith("«")))
            |> Seq.map sanitise
            |> Seq.filter (fun s -> not (String.IsNullOrEmpty(s)))
            |> Seq.take 3
            |> Seq.map normcase
            |> String.concat ""

        w.WriteLine ()
        w.WriteLine $"    // ID {id}"
        w.WriteLine $"    member _.{words}({pstr}) ="
        w.WriteLine $"        $\"{f}\""

let main =
    let productions = getProductions typeof<Command>
    let keywords = getProductionKeywords productions
    writeEbnf "generated/grammar.txt" productions
    writeParserFile "generated/arafel.fs" "Arafel" productions keywords
    writePrintFile "generated/pretty.fs" "Pretty" productions
    writeResources "en-us"
    0