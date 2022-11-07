module Arafel
// Generated code. Do not edit.
// Doing this instead of parser combinators for more straightforward debugging.

open Lexer
open Parse

let rec assign (q:TokenCursor) =
    let result = // 'let' ID ('(' ID (',' ID)* ')')? '=' expr
        let (at1, ac1) = Parse.isText "let" q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = Parse.isId ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = // ('(' ID (',' ID)* ')')?
            let (bt, bc) = // '(' ID (',' ID)* ')'
                let (ct1, cc1) = Parse.isText "(" ac2
                if ct1 = Error then (Error, ac2) else
                let (ct2, cc2) = Parse.isId cc1
                if ct2 = Error then (Error, ac2) else
                let (ct3, cc3) = // (',' ID)*
                    let rec z list (dq:TokenCursor) =
                        let (dt, dc) = // ',' ID
                            let (et1, ec1) = Parse.isText "," dq
                            if et1 = Error then (Error, dq) else
                            let (et2, ec2) = Parse.isId ec1
                            if et2 = Error then (Error, dq) else
                            (parseTreeFromList [et1; et2], ec2)
                        match dt with
                        | Error -> (parseTreeFromList (List.rev list), dq)
                        | _ -> z (dt :: list) dc
                    z [] cc2
                if ct3 = Error then (Error, ac2) else
                let (ct4, cc4) = Parse.isText ")" cc3
                if ct4 = Error then (Error, ac2) else
                (parseTreeFromList [ct1; ct2; ct3; ct4], cc4)
            match bt with
            | Error -> (Empty, ac2)
            | _ -> (bt, bc)
        if at3 = Error then (Error, q) else
        let (at4, ac4) = Parse.isText "=" ac3
        if at4 = Error then (Error, q) else
        let (at5, ac5) = expr ac4
        if at5 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3; at4; at5], ac5)
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
        let (at1, ac1) = // (assign | typeDecl)*
            let rec z list (bq:TokenCursor) =
                let (bt, bc) = // assign | typeDecl
                    let (ct1, cc1) = assign bq
                    if ct1 <> Error then (ct1, cc1) else
                    let (ct2, cc2) = typeDecl bq
                    if ct2 <> Error then (ct2, cc2) else
                    (Error, bq)
                match bt with
                | Error -> (parseTreeFromList (List.rev list), bq)
                | _ -> z (bt :: list) bc
            z [] q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = atom ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = // ('(' expr (',' expr)* ')')?
            let (bt, bc) = // '(' expr (',' expr)* ')'
                let (ct1, cc1) = Parse.isText "(" ac2
                if ct1 = Error then (Error, ac2) else
                let (ct2, cc2) = expr cc1
                if ct2 = Error then (Error, ac2) else
                let (ct3, cc3) = // (',' expr)*
                    let rec z list (dq:TokenCursor) =
                        let (dt, dc) = // ',' expr
                            let (et1, ec1) = Parse.isText "," dq
                            if et1 = Error then (Error, dq) else
                            let (et2, ec2) = expr ec1
                            if et2 = Error then (Error, dq) else
                            (parseTreeFromList [et1; et2], ec2)
                        match dt with
                        | Error -> (parseTreeFromList (List.rev list), dq)
                        | _ -> z (dt :: list) dc
                    z [] cc2
                if ct3 = Error then (Error, ac2) else
                let (ct4, cc4) = Parse.isText ")" cc3
                if ct4 = Error then (Error, ac2) else
                (parseTreeFromList [ct1; ct2; ct3; ct4], cc4)
            match bt with
            | Error -> (Empty, ac2)
            | _ -> (bt, bc)
        if at3 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3], ac3)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("expr", tree), next)

and lambda (q:TokenCursor) =
    let result = // '(' ID (',' ID)* ')' '=' expr
        let (at1, ac1) = Parse.isText "(" q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = Parse.isId ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = // (',' ID)*
            let rec z list (bq:TokenCursor) =
                let (bt, bc) = // ',' ID
                    let (ct1, cc1) = Parse.isText "," bq
                    if ct1 = Error then (Error, bq) else
                    let (ct2, cc2) = Parse.isId cc1
                    if ct2 = Error then (Error, bq) else
                    (parseTreeFromList [ct1; ct2], cc2)
                match bt with
                | Error -> (parseTreeFromList (List.rev list), bq)
                | _ -> z (bt :: list) bc
            z [] ac2
        if at3 = Error then (Error, q) else
        let (at4, ac4) = Parse.isText ")" ac3
        if at4 = Error then (Error, q) else
        let (at5, ac5) = Parse.isText "=" ac4
        if at5 = Error then (Error, q) else
        let (at6, ac6) = expr ac5
        if at6 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3; at4; at5; at6], ac6)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("lambda", tree), next)

and monoType (q:TokenCursor) =
    let result = // productType (('→' | '->') productType)*
        let (at1, ac1) = productType q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = // (('→' | '->') productType)*
            let rec z list (bq:TokenCursor) =
                let (bt, bc) = // ('→' | '->') productType
                    let (ct1, cc1) = // '→' | '->'
                        let (dt1, dc1) = Parse.isText "→" bq
                        if dt1 <> Error then (dt1, dc1) else
                        let (dt2, dc2) = Parse.isText "->" bq
                        if dt2 <> Error then (dt2, dc2) else
                        (Error, bq)
                    if ct1 = Error then (Error, bq) else
                    let (ct2, cc2) = productType cc1
                    if ct2 = Error then (Error, bq) else
                    (parseTreeFromList [ct1; ct2], cc2)
                match bt with
                | Error -> (parseTreeFromList (List.rev list), bq)
                | _ -> z (bt :: list) bc
            z [] ac1
        if at2 = Error then (Error, q) else
        (parseTreeFromList [at1; at2], ac2)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("monoType", tree), next)

and productType (q:TokenCursor) =
    let result = // typeAtom (('×' | '*') typeAtom)*
        let (at1, ac1) = typeAtom q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = // (('×' | '*') typeAtom)*
            let rec z list (bq:TokenCursor) =
                let (bt, bc) = // ('×' | '*') typeAtom
                    let (ct1, cc1) = // '×' | '*'
                        let (dt1, dc1) = Parse.isText "×" bq
                        if dt1 <> Error then (dt1, dc1) else
                        let (dt2, dc2) = Parse.isText "*" bq
                        if dt2 <> Error then (dt2, dc2) else
                        (Error, bq)
                    if ct1 = Error then (Error, bq) else
                    let (ct2, cc2) = typeAtom cc1
                    if ct2 = Error then (Error, bq) else
                    (parseTreeFromList [ct1; ct2], cc2)
                match bt with
                | Error -> (parseTreeFromList (List.rev list), bq)
                | _ -> z (bt :: list) bc
            z [] ac1
        if at2 = Error then (Error, q) else
        (parseTreeFromList [at1; at2], ac2)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("productType", tree), next)

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
    let result = // 'type' ID '=' tyƿe '.'
        let (at1, ac1) = Parse.isText "type" q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = Parse.isId ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = Parse.isText "=" ac2
        if at3 = Error then (Error, q) else
        let (at4, ac4) = tyƿe ac3
        if at4 = Error then (Error, q) else
        let (at5, ac5) = Parse.isText "." ac4
        if at5 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3; at4; at5], ac5)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("typeDecl", tree), next)

and tyƿe (q:TokenCursor) =
    let result = // (('∀' | 'forall') ID+ ',')? monoType
        let (at1, ac1) = // (('∀' | 'forall') ID+ ',')?
            let (bt, bc) = // ('∀' | 'forall') ID+ ','
                let (ct1, cc1) = // '∀' | 'forall'
                    let (dt1, dc1) = Parse.isText "∀" q
                    if dt1 <> Error then (dt1, dc1) else
                    let (dt2, dc2) = Parse.isText "forall" q
                    if dt2 <> Error then (dt2, dc2) else
                    (Error, q)
                if ct1 = Error then (Error, q) else
                let (ct2, cc2) = // ID+
                    let rec z list (dq:TokenCursor) =
                        let (dt, dc) = Parse.isId dq
                        match dt with
                        | Error -> (parseTreeFromList (List.rev list), dq)
                        | _ -> z (dt :: list) dc
                    match z [] cc1 with
                    | (Error, _) -> (Error, q)
                    | (Empty, _) -> (Error, q)
                    | (Node [], _) -> (Error, q)
                    | (tree, next) -> (tree, next)
                if ct2 = Error then (Error, q) else
                let (ct3, cc3) = Parse.isText "," cc2
                if ct3 = Error then (Error, q) else
                (parseTreeFromList [ct1; ct2; ct3], cc3)
            match bt with
            | Error -> (Empty, q)
            | _ -> (bt, bc)
        if at1 = Error then (Error, q) else
        let (at2, ac2) = monoType ac1
        if at2 = Error then (Error, q) else
        (parseTreeFromList [at1; at2], ac2)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("tyƿe", tree), next)

