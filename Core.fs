module Core

open Lexer
open Parse

let rec atom (q:TokenCursor) =
    let mutable ar = NoMatch
    ar <-
        match q.Current with
        | Id x -> Ok (Token q.Current, q.Next)
        | _ -> NoMatch
    if ar <> NoMatch then ar else
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
            if bq.Current = Id "`" then Ok (Token bq.Current, bq.Next) else NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            match bq.Current with
            | Operator x -> Ok (Token bq.Current, bq.Next)
            | _ -> NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            if bq.Current = Id "`" then Ok (Token bq.Current, bq.Next) else NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        Ok (Node (List.rev list), bq)
    if ar <> NoMatch then ar else
    ar <-
        let mutable list = []
        let mutable br = NoMatch
        let mutable bq = q
        br <-
            if bq.Current = Id "(" then Ok (Token bq.Current, bq.Next) else NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            expr bq
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            if bq.Current = Id ")" then Ok (Token bq.Current, bq.Next) else NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        Ok (Node (List.rev list), bq)
    if ar <> NoMatch then ar else
    ar <-
        lambda q
    if ar <> NoMatch then ar else
    ar <-
        let mutable list = []
        let mutable br = NoMatch
        let mutable bq = q
        br <-
            if bq.Current = Id "let" then Ok (Token bq.Current, bq.Next) else NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            typedVar bq
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            if bq.Current = Id "=" then Ok (Token bq.Current, bq.Next) else NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            expr bq
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            expr bq
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        Ok (Node (List.rev list), bq)
    if ar <> NoMatch then ar else
    ar <-
        case q
    if ar <> NoMatch then ar else
    NoMatch

and case (q:TokenCursor) =
    let mutable list = []
    let mutable ar = NoMatch
    let mutable aq = q
    ar <-
        if aq.Current = Id "case" then Ok (Token aq.Current, aq.Next) else NoMatch
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        expr aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        if aq.Current = Id "of" then Ok (Token aq.Current, aq.Next) else NoMatch
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        let z list (bq:TokenCursor) =
            let br =
                let mutable list = []
                let mutable cr = NoMatch
                let mutable cq = bq
                cr <-
                    pattern cq
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    cq <- next
                cr <-
                    let mutable dr = NoMatch
                    dr <-
                        if cq.Current = Id "→" then Ok (Token cq.Current, cq.Next) else NoMatch
                    if dr <> NoMatch then dr else
                    dr <-
                        if cq.Current = Id "->" then Ok (Token cq.Current, cq.Next) else NoMatch
                    if dr <> NoMatch then dr else
                    NoMatch
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    cq <- next
                cr <-
                    expr cq
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    cq <- next
                Ok (Node (List.rev list), cq)
            match br with
            | NoMatch -> Ok (Node (List.rev list), bq)
            | Ok (result, next) -> z (result :: list) next
        match z [] aq with
        | NoMatch -> NoMatch
        | Ok (Node [], _) -> NoMatch
        | Ok (result, next) -> Ok (result, next)
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    Ok (Node (List.rev list), aq)

and expr (q:TokenCursor) =
    let mutable list = []
    let mutable ar = NoMatch
    let mutable aq = q
    ar <-
        atom aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        let br =
            let mutable list = []
            let mutable cr = NoMatch
            let mutable cq = aq
            cr <-
                if cq.Current = Id "(" then Ok (Token cq.Current, cq.Next) else NoMatch
            if cr = NoMatch then NoMatch else
            match cr with
            | NoMatch -> ()
            | Ok (result, next) ->
                list <- result :: list
                cq <- next
            cr <-
                expr cq
            if cr = NoMatch then NoMatch else
            match cr with
            | NoMatch -> ()
            | Ok (result, next) ->
                list <- result :: list
                cq <- next
            cr <-
                let z list (dq:TokenCursor) =
                    let dr =
                        let mutable list = []
                        let mutable er = NoMatch
                        let mutable eq = dq
                        er <-
                            if eq.Current = Id "," then Ok (Token eq.Current, eq.Next) else NoMatch
                        if er = NoMatch then NoMatch else
                        match er with
                        | NoMatch -> ()
                        | Ok (result, next) ->
                            list <- result :: list
                            eq <- next
                        er <-
                            expr eq
                        if er = NoMatch then NoMatch else
                        match er with
                        | NoMatch -> ()
                        | Ok (result, next) ->
                            list <- result :: list
                            eq <- next
                        Ok (Node (List.rev list), eq)
                    match dr with
                    | NoMatch -> Ok (Node (List.rev list), dq)
                    | Ok (result, next) -> z (result :: list) next
                z [] cq
            if cr = NoMatch then NoMatch else
            match cr with
            | NoMatch -> ()
            | Ok (result, next) ->
                list <- result :: list
                cq <- next
            cr <-
                if cq.Current = Id ")" then Ok (Token cq.Current, cq.Next) else NoMatch
            if cr = NoMatch then NoMatch else
            match cr with
            | NoMatch -> ()
            | Ok (result, next) ->
                list <- result :: list
                cq <- next
            Ok (Node (List.rev list), cq)
        match br with
        | NoMatch -> Ok (Empty, aq)
        | Ok (result, next) -> Ok (result, next)
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    Ok (Node (List.rev list), aq)

and lambda (q:TokenCursor) =
    let mutable list = []
    let mutable ar = NoMatch
    let mutable aq = q
    ar <-
        if aq.Current = Id "(" then Ok (Token aq.Current, aq.Next) else NoMatch
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        typedVar aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        let z list (bq:TokenCursor) =
            let br =
                let mutable list = []
                let mutable cr = NoMatch
                let mutable cq = bq
                cr <-
                    if cq.Current = Id "," then Ok (Token cq.Current, cq.Next) else NoMatch
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    cq <- next
                cr <-
                    typedVar cq
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    cq <- next
                Ok (Node (List.rev list), cq)
            match br with
            | NoMatch -> Ok (Node (List.rev list), bq)
            | Ok (result, next) -> z (result :: list) next
        z [] aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        if aq.Current = Id ")" then Ok (Token aq.Current, aq.Next) else NoMatch
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        if aq.Current = Id "=" then Ok (Token aq.Current, aq.Next) else NoMatch
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        expr aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    Ok (Node (List.rev list), aq)

and monoType (q:TokenCursor) =
    let mutable list = []
    let mutable ar = NoMatch
    let mutable aq = q
    ar <-
        productType aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        let z list (bq:TokenCursor) =
            let br =
                let mutable list = []
                let mutable cr = NoMatch
                let mutable cq = bq
                cr <-
                    let mutable dr = NoMatch
                    dr <-
                        if cq.Current = Id "→" then Ok (Token cq.Current, cq.Next) else NoMatch
                    if dr <> NoMatch then dr else
                    dr <-
                        if cq.Current = Id "->" then Ok (Token cq.Current, cq.Next) else NoMatch
                    if dr <> NoMatch then dr else
                    NoMatch
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    cq <- next
                cr <-
                    productType cq
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    cq <- next
                Ok (Node (List.rev list), cq)
            match br with
            | NoMatch -> Ok (Node (List.rev list), bq)
            | Ok (result, next) -> z (result :: list) next
        z [] aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    Ok (Node (List.rev list), aq)

and pattern (q:TokenCursor) =
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
            match bq.Current with
            | Id x -> Ok (Token bq.Current, bq.Next)
            | _ -> NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            let cr =
                let mutable list = []
                let mutable dr = NoMatch
                let mutable dq = bq
                dr <-
                    if dq.Current = Id "(" then Ok (Token dq.Current, dq.Next) else NoMatch
                if dr = NoMatch then NoMatch else
                match dr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    dq <- next
                dr <-
                    match dq.Current with
                    | Id x -> Ok (Token dq.Current, dq.Next)
                    | _ -> NoMatch
                if dr = NoMatch then NoMatch else
                match dr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    dq <- next
                dr <-
                    let z list (eq:TokenCursor) =
                        let er =
                            let mutable list = []
                            let mutable fr = NoMatch
                            let mutable fq = eq
                            fr <-
                                if fq.Current = Id "," then Ok (Token fq.Current, fq.Next) else NoMatch
                            if fr = NoMatch then NoMatch else
                            match fr with
                            | NoMatch -> ()
                            | Ok (result, next) ->
                                list <- result :: list
                                fq <- next
                            fr <-
                                match fq.Current with
                                | Id x -> Ok (Token fq.Current, fq.Next)
                                | _ -> NoMatch
                            if fr = NoMatch then NoMatch else
                            match fr with
                            | NoMatch -> ()
                            | Ok (result, next) ->
                                list <- result :: list
                                fq <- next
                            Ok (Node (List.rev list), fq)
                        match er with
                        | NoMatch -> Ok (Node (List.rev list), eq)
                        | Ok (result, next) -> z (result :: list) next
                    z [] dq
                if dr = NoMatch then NoMatch else
                match dr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    dq <- next
                dr <-
                    if dq.Current = Id ")" then Ok (Token dq.Current, dq.Next) else NoMatch
                if dr = NoMatch then NoMatch else
                match dr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    dq <- next
                Ok (Node (List.rev list), dq)
            match cr with
            | NoMatch -> Ok (Empty, bq)
            | Ok (result, next) -> Ok (result, next)
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        Ok (Node (List.rev list), bq)
    if ar <> NoMatch then ar else
    NoMatch

and productType (q:TokenCursor) =
    let mutable list = []
    let mutable ar = NoMatch
    let mutable aq = q
    ar <-
        typeAtom aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        let z list (bq:TokenCursor) =
            let br =
                let mutable list = []
                let mutable cr = NoMatch
                let mutable cq = bq
                cr <-
                    let mutable dr = NoMatch
                    dr <-
                        if cq.Current = Id "×" then Ok (Token cq.Current, cq.Next) else NoMatch
                    if dr <> NoMatch then dr else
                    dr <-
                        if cq.Current = Id "*" then Ok (Token cq.Current, cq.Next) else NoMatch
                    if dr <> NoMatch then dr else
                    NoMatch
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    cq <- next
                cr <-
                    typeAtom cq
                if cr = NoMatch then NoMatch else
                match cr with
                | NoMatch -> ()
                | Ok (result, next) ->
                    list <- result :: list
                    cq <- next
                Ok (Node (List.rev list), cq)
            match br with
            | NoMatch -> Ok (Node (List.rev list), bq)
            | Ok (result, next) -> z (result :: list) next
        z [] aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    Ok (Node (List.rev list), aq)

and typ (q:TokenCursor) =
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
                    if cq.Current = Id "∀" then Ok (Token cq.Current, cq.Next) else NoMatch
                if dr <> NoMatch then dr else
                dr <-
                    if cq.Current = Id "forall" then Ok (Token cq.Current, cq.Next) else NoMatch
                if dr <> NoMatch then dr else
                NoMatch
            if cr = NoMatch then NoMatch else
            match cr with
            | NoMatch -> ()
            | Ok (result, next) ->
                list <- result :: list
                cq <- next
            cr <-
                let z list (dq:TokenCursor) =
                    let dr =
                        match dq.Current with
                        | Id x -> Ok (Token dq.Current, dq.Next)
                        | _ -> NoMatch
                    match dr with
                    | NoMatch -> Ok (Node (List.rev list), dq)
                    | Ok (result, next) -> z (result :: list) next
                match z [] cq with
                | NoMatch -> ignore
                | Ok (Node [], _) -> NoMatch
                | Ok (result, next) -> Ok (result, next)
            if cr = NoMatch then NoMatch else
            match cr with
            | NoMatch -> ()
            | Ok (result, next) ->
                list <- result :: list
                cq <- next
            cr <-
                if cq.Current = Id "." then Ok (Token cq.Current, cq.Next) else NoMatch
            if cr = NoMatch then NoMatch else
            match cr with
            | NoMatch -> ()
            | Ok (result, next) ->
                list <- result :: list
                cq <- next
            Ok (Node (List.rev list), cq)
        match br with
        | NoMatch -> Ok (Empty, aq)
        | Ok (result, next) -> Ok (result, next)
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        monoType aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    Ok (Node (List.rev list), aq)

and typeAtom (q:TokenCursor) =
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
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            let z list (cq:TokenCursor) =
                let cr =
                    monoType cq
                match cr with
                | NoMatch -> Ok (Node (List.rev list), cq)
                | Ok (result, next) -> z (result :: list) next
            z [] bq
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        Ok (Node (List.rev list), bq)
    if ar <> NoMatch then ar else
    ar <-
        let mutable list = []
        let mutable br = NoMatch
        let mutable bq = q
        br <-
            if bq.Current = Id "(" then Ok (Token bq.Current, bq.Next) else NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            monoType bq
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        br <-
            if bq.Current = Id ")" then Ok (Token bq.Current, bq.Next) else NoMatch
        if br = NoMatch then NoMatch else
        match br with
        | NoMatch -> ()
        | Ok (result, next) ->
            list <- result :: list
            bq <- next
        Ok (Node (List.rev list), bq)
    if ar <> NoMatch then ar else
    NoMatch

and typedVar (q:TokenCursor) =
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
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        if aq.Current = Id ":" then Ok (Token aq.Current, aq.Next) else NoMatch
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    ar <-
        typ aq
    if ar = NoMatch then NoMatch else
    match ar with
    | NoMatch -> ()
    | Ok (result, next) ->
        list <- result :: list
        aq <- next
    Ok (Node (List.rev list), aq)

