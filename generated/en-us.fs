module Localisation_en_us
// Generated code. Do not edit.

open System

type Strings() =

    abstract member CapabilityNotYet : unit -> string
    default _.CapabilityNotYet() =
        $"Capability not yet implemented."

    abstract member EndOfText : Object -> string
    default _.EndOfText(ex) =
        $"End of text: Expected «{ex}»."

    abstract member EnterToQuit : unit -> string
    default _.EnterToQuit() =
        $"Enter 'quit' to quit. End lines with '\\' for multiline."

    abstract member IsAlreadyBound : Object -> string
    default _.IsAlreadyBound(id) =
        $"«{id}» is already bound to a value."

    abstract member IsNotBound : Object -> string
    default _.IsNotBound(id) =
        $"«{id}» is not bound to a value."

    abstract member LinePosToken : Object * Object * Object * Object -> string
    default _.LinePosToken(line, pos, tx, ex) =
        $"Line «{line}» Pos «{pos}» Token «{tx}»: Expected «{ex}»."

    abstract member OperatorNotBound : Object -> string
    default _.OperatorNotBound(opname) =
        $"Operator «{opname}» not bound to a function."

    abstract member TooManyArguments : Object * Object * Object -> string
    default _.TooManyArguments(argc, opname, cmax) =
        $"Too many arguments «{argc}» given for function «{opname}» which accepts no more than «{cmax}»."
