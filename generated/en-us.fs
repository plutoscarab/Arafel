module Localisation_en_us

open System

type Strings() =

    // ID 0
    abstract member EndOfText : Object -> string
    default _.EndOfText(ex) =
        $"End of text: Expected «{ex}»."

    // ID 1
    abstract member LinePosToken : Object * Object * Object * Object -> string
    default _.LinePosToken(line, pos, tx, ex) =
        $"Line «{line}» Pos «{pos}» Token «{tx}»: Expected «{ex}»."

    // ID 2
    abstract member IsNotBound : Object -> string
    default _.IsNotBound(id) =
        $"«{id}» is not bound to a value."

    // ID 3
    abstract member TooManyArguments : Object * Object * Object -> string
    default _.TooManyArguments(argc, opname, cmax) =
        $"Too many arguments «{argc}» given for function «{opname}» which accepts no more than «{cmax}»."

    // ID 4
    abstract member IsAlreadyBound : Object -> string
    default _.IsAlreadyBound(id) =
        $"«{id}» is already bound to a value."

    // ID 5
    abstract member EnterQuitTo : unit -> string
    default _.EnterQuitTo() =
        $"Enter 'quit' to quit. End lines with '\\' for multiline."

    // ID 6
    abstract member CapabilityNotYet : unit -> string
    default _.CapabilityNotYet() =
        $"Capability not yet implemented."

    // ID 7
    abstract member OperatorNotBound : Object -> string
    default _.OperatorNotBound(opname) =
        $"Operator «{opname}» not bound to a function."
