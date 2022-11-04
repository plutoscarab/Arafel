module Core
// Generated code. Do not edit.
// Doing this instead of parser combinators for more straightforward debugging.

open Lexer
open Parse

let rec assign (q:TokenCursor) =
    let result =
        let mutable list = []
        let mutable ar = NoMatch
        let mutable aq = q
        ar <-
            if tokenText (aq.Current) = "let" then Ok (Token aq.Current, aq.Next) else NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            match aq.Current with
            | Id x -> Ok (Token aq.Current, aq.Next)
            | _ -> NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            if tokenText (aq.Current) = "=" then Ok (Token aq.Current, aq.Next) else NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            expr aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        Ok (parseTreeFromList (List.rev list), aq)
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("assign", tree), next)

and atom (q:TokenCursor) =
    let result =
        let mutable ar = NoMatch
        ar <-
            match q.Current with
            | Nat x -> Ok (Token q.Current, q.Next)
            | _ -> NoMatch
        if ar <> NoMatch then ar else
        ar <-
            match q.Current with
            | String x -> Ok (Token q.Current, q.Next)
            | _ -> NoMatch
        if ar <> NoMatch then ar else
        ar <-
            let mutable list = []
            let mutable br = NoMatch
            let mutable bq = q
            br <-
                if tokenText (bq.Current) = "`" then Ok (Token bq.Current, bq.Next) else NoMatch
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            br <-
                match bq.Current with
                | Operator x -> Ok (Token bq.Current, bq.Next)
                | _ -> NoMatch
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            br <-
                if tokenText (bq.Current) = "`" then Ok (Token bq.Current, bq.Next) else NoMatch
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            Ok (parseTreeFromList (List.rev list), bq)
        if ar <> NoMatch then ar else
        ar <-
            lambda q
        if ar <> NoMatch then ar else
        ar <-
            let mutable list = []
            let mutable br = NoMatch
            let mutable bq = q
            br <-
                if tokenText (bq.Current) = "(" then Ok (Token bq.Current, bq.Next) else NoMatch
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            br <-
                expr bq
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            br <-
                if tokenText (bq.Current) = ")" then Ok (Token bq.Current, bq.Next) else NoMatch
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            Ok (parseTreeFromList (List.rev list), bq)
        if ar <> NoMatch then ar else
        ar <-
            match q.Current with
            | Id x -> Ok (Token q.Current, q.Next)
            | _ -> NoMatch
        if ar <> NoMatch then ar else
        NoMatch
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("atom", tree), next)

and expr (q:TokenCursor) =
    let result =
        let mutable list = []
        let mutable ar = NoMatch
        let mutable aq = q
        ar <-
            let rec z list (bq:TokenCursor) =
                let br =
                    let mutable cr = NoMatch
                    cr <-
                        assign bq
                    if cr <> NoMatch then cr else
                    cr <-
                        typeDecl bq
                    if cr <> NoMatch then cr else
                    NoMatch
                match br with
                | NoMatch -> Ok (parseTreeFromList (List.rev list), bq)
                | Ok (tree, next) -> z (tree :: list) next
            z [] aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            atom aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            let br =
                let mutable list = []
                let mutable cr = NoMatch
                let mutable cq = aq
                cr <-
                    if tokenText (cq.Current) = "(" then Ok (Token cq.Current, cq.Next) else NoMatch
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (tree, next) ->
                    list <- tree :: list
                    cq <- next
                cr <-
                    expr cq
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (tree, next) ->
                    list <- tree :: list
                    cq <- next
                cr <-
                    let rec z list (dq:TokenCursor) =
                        let dr =
                            let mutable list = []
                            let mutable er = NoMatch
                            let mutable eq = dq
                            er <-
                                if tokenText (eq.Current) = "," then Ok (Token eq.Current, eq.Next) else NoMatch
                            if er = NoMatch then NoMatch else
                            match er with
                            | NoMatch -> ()
                            | Ok (tree, next) ->
                                list <- tree :: list
                                eq <- next
                            er <-
                                expr eq
                            if er = NoMatch then NoMatch else
                            match er with
                            | NoMatch -> ()
                            | Ok (tree, next) ->
                                list <- tree :: list
                                eq <- next
                            Ok (parseTreeFromList (List.rev list), eq)
                        match dr with
                        | NoMatch -> Ok (parseTreeFromList (List.rev list), dq)
                        | Ok (tree, next) -> z (tree :: list) next
                    z [] cq
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (tree, next) ->
                    list <- tree :: list
                    cq <- next
                cr <-
                    if tokenText (cq.Current) = ")" then Ok (Token cq.Current, cq.Next) else NoMatch
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (tree, next) ->
                    list <- tree :: list
                    cq <- next
                Ok (parseTreeFromList (List.rev list), cq)
            match br with
            | NoMatch -> Ok (Empty, aq)
            | Ok (tree, next) -> Ok (tree, next)
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        Ok (parseTreeFromList (List.rev list), aq)
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("expr", tree), next)

and lambda (q:TokenCursor) =
    let result =
        let mutable list = []
        let mutable ar = NoMatch
        let mutable aq = q
        ar <-
            if tokenText (aq.Current) = "(" then Ok (Token aq.Current, aq.Next) else NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            typedVar aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            let rec z list (bq:TokenCursor) =
                let br =
                    let mutable list = []
                    let mutable cr = NoMatch
                    let mutable cq = bq
                    cr <-
                        if tokenText (cq.Current) = "," then Ok (Token cq.Current, cq.Next) else NoMatch
                    if cr = NoMatch then NoMatch else
                    match cr with
                    | NoMatch -> ()
                    | Ok (tree, next) ->
                        list <- tree :: list
                        cq <- next
                    cr <-
                        typedVar cq
                    if cr = NoMatch then NoMatch else
                    match cr with
                    | NoMatch -> ()
                    | Ok (tree, next) ->
                        list <- tree :: list
                        cq <- next
                    Ok (parseTreeFromList (List.rev list), cq)
                match br with
                | NoMatch -> Ok (parseTreeFromList (List.rev list), bq)
                | Ok (tree, next) -> z (tree :: list) next
            z [] aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            if tokenText (aq.Current) = ")" then Ok (Token aq.Current, aq.Next) else NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            if tokenText (aq.Current) = "=" then Ok (Token aq.Current, aq.Next) else NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            expr aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        Ok (parseTreeFromList (List.rev list), aq)
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("lambda", tree), next)

and monoType (q:TokenCursor) =
    let result =
        let mutable list = []
        let mutable ar = NoMatch
        let mutable aq = q
        ar <-
            productType aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            let rec z list (bq:TokenCursor) =
                let br =
                    let mutable list = []
                    let mutable cr = NoMatch
                    let mutable cq = bq
                    cr <-
                        let mutable dr = NoMatch
                        dr <-
                            if tokenText (cq.Current) = "→" then Ok (Token cq.Current, cq.Next) else NoMatch
                        if dr <> NoMatch then dr else
                        dr <-
                            if tokenText (cq.Current) = "->" then Ok (Token cq.Current, cq.Next) else NoMatch
                        if dr <> NoMatch then dr else
                        NoMatch
                    if cr = NoMatch then NoMatch else
                    match cr with
                    | NoMatch -> ()
                    | Ok (tree, next) ->
                        list <- tree :: list
                        cq <- next
                    cr <-
                        productType cq
                    if cr = NoMatch then NoMatch else
                    match cr with
                    | NoMatch -> ()
                    | Ok (tree, next) ->
                        list <- tree :: list
                        cq <- next
                    Ok (parseTreeFromList (List.rev list), cq)
                match br with
                | NoMatch -> Ok (parseTreeFromList (List.rev list), bq)
                | Ok (tree, next) -> z (tree :: list) next
            z [] aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        Ok (parseTreeFromList (List.rev list), aq)
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("monoType", tree), next)

and productType (q:TokenCursor) =
    let result =
        let mutable list = []
        let mutable ar = NoMatch
        let mutable aq = q
        ar <-
            typeAtom aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            let rec z list (bq:TokenCursor) =
                let br =
                    let mutable list = []
                    let mutable cr = NoMatch
                    let mutable cq = bq
                    cr <-
                        let mutable dr = NoMatch
                        dr <-
                            if tokenText (cq.Current) = "×" then Ok (Token cq.Current, cq.Next) else NoMatch
                        if dr <> NoMatch then dr else
                        dr <-
                            if tokenText (cq.Current) = "*" then Ok (Token cq.Current, cq.Next) else NoMatch
                        if dr <> NoMatch then dr else
                        NoMatch
                    if cr = NoMatch then NoMatch else
                    match cr with
                    | NoMatch -> ()
                    | Ok (tree, next) ->
                        list <- tree :: list
                        cq <- next
                    cr <-
                        typeAtom cq
                    if cr = NoMatch then NoMatch else
                    match cr with
                    | NoMatch -> ()
                    | Ok (tree, next) ->
                        list <- tree :: list
                        cq <- next
                    Ok (parseTreeFromList (List.rev list), cq)
                match br with
                | NoMatch -> Ok (parseTreeFromList (List.rev list), bq)
                | Ok (tree, next) -> z (tree :: list) next
            z [] aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        Ok (parseTreeFromList (List.rev list), aq)
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("productType", tree), next)

and typ (q:TokenCursor) =
    let result =
        let mutable list = []
        let mutable ar = NoMatch
        let mutable aq = q
        ar <-
            let br =
                let mutable list = []
                let mutable cr = NoMatch
                let mutable cq = aq
                cr <-
                    let mutable dr = NoMatch
                    dr <-
                        if tokenText (cq.Current) = "∀" then Ok (Token cq.Current, cq.Next) else NoMatch
                    if dr <> NoMatch then dr else
                    dr <-
                        if tokenText (cq.Current) = "forall" then Ok (Token cq.Current, cq.Next) else NoMatch
                    if dr <> NoMatch then dr else
                    NoMatch
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (tree, next) ->
                    list <- tree :: list
                    cq <- next
                cr <-
                    let rec z list (dq:TokenCursor) =
                        let dr =
                            match dq.Current with
                            | Id x -> Ok (Token dq.Current, dq.Next)
                            | _ -> NoMatch
                        match dr with
                        | NoMatch -> Ok (parseTreeFromList (List.rev list), dq)
                        | Ok (tree, next) -> z (tree :: list) next
                    match z [] cq with
                    | NoMatch -> NoMatch
                    | Ok (Empty, _) -> NoMatch
                    | Ok (Node [], _) -> NoMatch
                    | Ok (tree, next) -> Ok (tree, next)
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (tree, next) ->
                    list <- tree :: list
                    cq <- next
                cr <-
                    if tokenText (cq.Current) = "," then Ok (Token cq.Current, cq.Next) else NoMatch
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (tree, next) ->
                    list <- tree :: list
                    cq <- next
                Ok (parseTreeFromList (List.rev list), cq)
            match br with
            | NoMatch -> Ok (Empty, aq)
            | Ok (tree, next) -> Ok (tree, next)
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            monoType aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        Ok (parseTreeFromList (List.rev list), aq)
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("typ", tree), next)

and typeAtom (q:TokenCursor) =
    let result =
        let mutable ar = NoMatch
        ar <-
            let mutable list = []
            let mutable br = NoMatch
            let mutable bq = q
            br <-
                match bq.Current with
                | Id x -> Ok (Token bq.Current, bq.Next)
                | _ -> NoMatch
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            br <-
                let rec z list (cq:TokenCursor) =
                    let cr =
                        monoType cq
                    match cr with
                    | NoMatch -> Ok (parseTreeFromList (List.rev list), cq)
                    | Ok (tree, next) -> z (tree :: list) next
                z [] bq
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            Ok (parseTreeFromList (List.rev list), bq)
        if ar <> NoMatch then ar else
        ar <-
            let mutable list = []
            let mutable br = NoMatch
            let mutable bq = q
            br <-
                if tokenText (bq.Current) = "(" then Ok (Token bq.Current, bq.Next) else NoMatch
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            br <-
                monoType bq
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            br <-
                if tokenText (bq.Current) = ")" then Ok (Token bq.Current, bq.Next) else NoMatch
            if br = NoMatch then NoMatch else
            match br with
            | NoMatch -> ()
            | Ok (tree, next) ->
                list <- tree :: list
                bq <- next
            Ok (parseTreeFromList (List.rev list), bq)
        if ar <> NoMatch then ar else
        NoMatch
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("typeAtom", tree), next)

and typeDecl (q:TokenCursor) =
    let result =
        let mutable list = []
        let mutable ar = NoMatch
        let mutable aq = q
        ar <-
            if tokenText (aq.Current) = "type" then Ok (Token aq.Current, aq.Next) else NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            match aq.Current with
            | Id x -> Ok (Token aq.Current, aq.Next)
            | _ -> NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            if tokenText (aq.Current) = "=" then Ok (Token aq.Current, aq.Next) else NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            typ aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            if tokenText (aq.Current) = "." then Ok (Token aq.Current, aq.Next) else NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        Ok (parseTreeFromList (List.rev list), aq)
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("typeDecl", tree), next)

and typedVar (q:TokenCursor) =
    let result =
        let mutable list = []
        let mutable ar = NoMatch
        let mutable aq = q
        ar <-
            match aq.Current with
            | Id x -> Ok (Token aq.Current, aq.Next)
            | _ -> NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            if tokenText (aq.Current) = ":" then Ok (Token aq.Current, aq.Next) else NoMatch
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        ar <-
            typ aq
        if ar = NoMatch then NoMatch else
        match ar with
        | NoMatch -> ()
        | Ok (tree, next) ->
            list <- tree :: list
            aq <- next
        Ok (parseTreeFromList (List.rev list), aq)
    match result with
    | NoMatch -> NoMatch
    | Ok (tree, next) -> Ok (Production ("typedVar", tree), next)

