module Localisation_en_us

type Strings() =

    // ID 0
    member _.EndOfText(ex) =
        $"End of text: Expected «{ex}»."

    // ID 1
    member _.LinePosToken(line, pos, tx, ex) =
        $"Line «{line}» Pos «{pos}» Token «{tx}»: Expected «{ex}»."

    // ID 2
    member _.IsNotBound(id) =
        $"«{id}» is not bound to a value."

    // ID 3
    member _.TooManyArguments(argc, opname, cmax) =
        $"Too many arguments «{argc}» given for function «{opname}» which accepts no more than «{cmax}»."

    // ID 4
    member _.IsAlreadyBound(id) =
        $"«{id}» is already bound to a value."

    // ID 5
    member _.EnterQuitTo() =
        $"Enter 'quit' to quit. End lines with '\\' for multiline."

    // ID 6
    member _.CapabilityNotYet() =
        $"Capability not yet implemented."

    // ID 7
    member _.OperatorNotBound(opname) =
        $"Operator «{opname}» not bound to a function."
