open Syntax
open Reflect
open Ebnf
open Parse
open Print

let main =
    let productions = getProductions typeof<Command>
    let keywords = getProductionKeywords productions
    writeEbnf "generated/grammar.txt" productions
    writeParserFile "generated/arafel.fs" "Arafel" productions keywords
    writePrintFile "generated/pretty.fs" "Pretty" productions
    0