module Core
// Generated code. Do not edit.
// Doing this instead of parser combinators for more straightforward debugging.

open Lexer
open Parse

let rec assign (q:TokenCursor) =
    let result = // 'let' ID '=' expr
        let (at1, ac1) = // 'let' ID '=' expr
            let (bt1, bc1) = Parse.isText "let" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = Parse.isId bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = Parse.isText "=" bc2
            if bt3 = Error then (Error, q) else
            let (bt4, bc4) = expr bc3
            if bt4 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3; bt4], bc4)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("assign", tree), next)

and atom (q:TokenCursor) =
    let result = // NAT | STRING | '`' OPERATOR '`' | lambda | '(' expr ')' | ID
        let (at1, ac1) = Parse.isNat q
        if at1 <> Error then (at1, ac1) else
        let (at2, ac2) = Parse.isString q
        if at2 <> Error then (at2, ac2) else
        let (at3, ac3) = // '`' OPERATOR '`'
            let (bt1, bc1) = Parse.isText "`" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = Parse.isOperator bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = Parse.isText "`" bc2
            if bt3 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3], bc3)
        if at3 <> Error then (at3, ac3) else
        let (at4, ac4) = lambda q
        if at4 <> Error then (at4, ac4) else
        let (at5, ac5) = // '(' expr ')'
            let (bt1, bc1) = Parse.isText "(" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = expr bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = Parse.isText ")" bc2
            if bt3 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3], bc3)
        if at5 <> Error then (at5, ac5) else
        let (at6, ac6) = Parse.isId q
        if at6 <> Error then (at6, ac6) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("atom", tree), next)

and expr (q:TokenCursor) =
    let result = // (assign | typeDecl)* atom ('(' expr (',' expr)* ')')?
        let (at1, ac1) = // (assign | typeDecl)* atom ('(' expr (',' expr)* ')')?
            let (bt1, bc1) = // (assign | typeDecl)*
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = // assign | typeDecl
                        let (dt1, dc1) = assign cq
                        if dt1 <> Error then (dt1, dc1) else
                        let (dt2, dc2) = typeDecl cq
                        if dt2 <> Error then (dt2, dc2) else
                        (Error, cq)
                    match ct with
                    | Error -> (parseTreeFromList (List.rev list), cq)
                    | _ -> z (ct :: list) cc
                z [] q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = atom bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = // ('(' expr (',' expr)* ')')?
                let (ct, cc) = // '(' expr (',' expr)* ')'
                    let (dt1, dc1) = // '(' expr (',' expr)* ')'
                        let (et1, ec1) = Parse.isText "(" bc2
                        if et1 = Error then (Error, bc2) else
                        let (et2, ec2) = expr ec1
                        if et2 = Error then (Error, bc2) else
                        let (et3, ec3) = // (',' expr)*
                            let rec z list (fq:TokenCursor) =
                                let (ft, fc) = // ',' expr
                                    let (gt1, gc1) = // ',' expr
                                        let (ht1, hc1) = Parse.isText "," fq
                                        if ht1 = Error then (Error, fq) else
                                        let (ht2, hc2) = expr hc1
                                        if ht2 = Error then (Error, fq) else
                                        (parseTreeFromList [ht1; ht2], hc2)
                                    if gt1 <> Error then (gt1, gc1) else
                                    (Error, fq)
                                match ft with
                                | Error -> (parseTreeFromList (List.rev list), fq)
                                | _ -> z (ft :: list) fc
                            z [] ec2
                        if et3 = Error then (Error, bc2) else
                        let (et4, ec4) = Parse.isText ")" ec3
                        if et4 = Error then (Error, bc2) else
                        (parseTreeFromList [et1; et2; et3; et4], ec4)
                    if dt1 <> Error then (dt1, dc1) else
                    (Error, bc2)
                match ct with
                | Error -> (Empty, bc2)
                | _ -> (ct, cc)
            if bt3 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3], bc3)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("expr", tree), next)

and lambda (q:TokenCursor) =
    let result = // '(' typedVar (',' typedVar)* ')' '=' expr
        let (at1, ac1) = // '(' typedVar (',' typedVar)* ')' '=' expr
            let (bt1, bc1) = Parse.isText "(" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = typedVar bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = // (',' typedVar)*
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = // ',' typedVar
                        let (dt1, dc1) = // ',' typedVar
                            let (et1, ec1) = Parse.isText "," cq
                            if et1 = Error then (Error, cq) else
                            let (et2, ec2) = typedVar ec1
                            if et2 = Error then (Error, cq) else
                            (parseTreeFromList [et1; et2], ec2)
                        if dt1 <> Error then (dt1, dc1) else
                        (Error, cq)
                    match ct with
                    | Error -> (parseTreeFromList (List.rev list), cq)
                    | _ -> z (ct :: list) cc
                z [] bc2
            if bt3 = Error then (Error, q) else
            let (bt4, bc4) = Parse.isText ")" bc3
            if bt4 = Error then (Error, q) else
            let (bt5, bc5) = Parse.isText "=" bc4
            if bt5 = Error then (Error, q) else
            let (bt6, bc6) = expr bc5
            if bt6 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3; bt4; bt5; bt6], bc6)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("lambda", tree), next)

and monoType (q:TokenCursor) =
    let result = // productType (('→' | '->') productType)*
        let (at1, ac1) = // productType (('→' | '->') productType)*
            let (bt1, bc1) = productType q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = // (('→' | '->') productType)*
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = // ('→' | '->') productType
                        let (dt1, dc1) = // ('→' | '->') productType
                            let (et1, ec1) = // '→' | '->'
                                let (ft1, fc1) = Parse.isText "→" cq
                                if ft1 <> Error then (ft1, fc1) else
                                let (ft2, fc2) = Parse.isText "->" cq
                                if ft2 <> Error then (ft2, fc2) else
                                (Error, cq)
                            if et1 = Error then (Error, cq) else
                            let (et2, ec2) = productType ec1
                            if et2 = Error then (Error, cq) else
                            (parseTreeFromList [et1; et2], ec2)
                        if dt1 <> Error then (dt1, dc1) else
                        (Error, cq)
                    match ct with
                    | Error -> (parseTreeFromList (List.rev list), cq)
                    | _ -> z (ct :: list) cc
                z [] bc1
            if bt2 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2], bc2)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("monoType", tree), next)

and productType (q:TokenCursor) =
    let result = // typeAtom (('×' | '*') typeAtom)*
        let (at1, ac1) = // typeAtom (('×' | '*') typeAtom)*
            let (bt1, bc1) = typeAtom q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = // (('×' | '*') typeAtom)*
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = // ('×' | '*') typeAtom
                        let (dt1, dc1) = // ('×' | '*') typeAtom
                            let (et1, ec1) = // '×' | '*'
                                let (ft1, fc1) = Parse.isText "×" cq
                                if ft1 <> Error then (ft1, fc1) else
                                let (ft2, fc2) = Parse.isText "*" cq
                                if ft2 <> Error then (ft2, fc2) else
                                (Error, cq)
                            if et1 = Error then (Error, cq) else
                            let (et2, ec2) = typeAtom ec1
                            if et2 = Error then (Error, cq) else
                            (parseTreeFromList [et1; et2], ec2)
                        if dt1 <> Error then (dt1, dc1) else
                        (Error, cq)
                    match ct with
                    | Error -> (parseTreeFromList (List.rev list), cq)
                    | _ -> z (ct :: list) cc
                z [] bc1
            if bt2 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2], bc2)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("productType", tree), next)

and typ (q:TokenCursor) =
    let result = // (('∀' | 'forall') ID+ ',')? monoType
        let (at1, ac1) = // (('∀' | 'forall') ID+ ',')? monoType
            let (bt1, bc1) = // (('∀' | 'forall') ID+ ',')?
                let (ct, cc) = // ('∀' | 'forall') ID+ ','
                    let (dt1, dc1) = // ('∀' | 'forall') ID+ ','
                        let (et1, ec1) = // '∀' | 'forall'
                            let (ft1, fc1) = Parse.isText "∀" q
                            if ft1 <> Error then (ft1, fc1) else
                            let (ft2, fc2) = Parse.isText "forall" q
                            if ft2 <> Error then (ft2, fc2) else
                            (Error, q)
                        if et1 = Error then (Error, q) else
                        let (et2, ec2) = // ID+
                            let rec z list (fq:TokenCursor) =
                                let (ft, fc) = Parse.isId fq
                                match ft with
                                | Error -> (parseTreeFromList (List.rev list), fq)
                                | _ -> z (ft :: list) fc
                            match z [] ec1 with
                            | (Error, _) -> (Error, q)
                            | (Empty, _) -> (Error, q)
                            | (Node [], _) -> (Error, q)
                            | (tree, next) -> (tree, next)
                        if et2 = Error then (Error, q) else
                        let (et3, ec3) = Parse.isText "," ec2
                        if et3 = Error then (Error, q) else
                        (parseTreeFromList [et1; et2; et3], ec3)
                    if dt1 <> Error then (dt1, dc1) else
                    (Error, q)
                match ct with
                | Error -> (Empty, q)
                | _ -> (ct, cc)
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = monoType bc1
            if bt2 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2], bc2)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("typ", tree), next)

and typeAtom (q:TokenCursor) =
    let result = // ID monoType* | '(' monoType ')'
        let (at1, ac1) = // ID monoType*
            let (bt1, bc1) = Parse.isId q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = // monoType*
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = monoType cq
                    match ct with
                    | Error -> (parseTreeFromList (List.rev list), cq)
                    | _ -> z (ct :: list) cc
                z [] bc1
            if bt2 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2], bc2)
        if at1 <> Error then (at1, ac1) else
        let (at2, ac2) = // '(' monoType ')'
            let (bt1, bc1) = Parse.isText "(" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = monoType bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = Parse.isText ")" bc2
            if bt3 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3], bc3)
        if at2 <> Error then (at2, ac2) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("typeAtom", tree), next)

and typeDecl (q:TokenCursor) =
    let result = // 'type' ID '=' typ '.'
        let (at1, ac1) = // 'type' ID '=' typ '.'
            let (bt1, bc1) = Parse.isText "type" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = Parse.isId bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = Parse.isText "=" bc2
            if bt3 = Error then (Error, q) else
            let (bt4, bc4) = typ bc3
            if bt4 = Error then (Error, q) else
            let (bt5, bc5) = Parse.isText "." bc4
            if bt5 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3; bt4; bt5], bc5)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("typeDecl", tree), next)

and typedVar (q:TokenCursor) =
    let result = // ID ':' typ
        let (at1, ac1) = // ID ':' typ
            let (bt1, bc1) = Parse.isId q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = Parse.isText ":" bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = typ bc2
            if bt3 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3], bc3)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("typedVar", tree), next)

