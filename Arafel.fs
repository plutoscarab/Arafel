module Arafel
// Generated code. Do not edit.
// Doing this instead of parser combinators for more straightforward debugging.

open Lexer
open Parse

let rec args (q:TokenCursor) =
    let result = // '(' expr (',' expr)* ')'
        let (at1, ac1) = Parse.isText "(" q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = expr ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = // (',' expr)*
            let rec z list (bq:TokenCursor) =
                let (bt, bc) = // ',' expr
                    let (ct1, cc1) = Parse.isText "," bq
                    if ct1 = Error then (Error, bq) else
                    let (ct2, cc2) = expr cc1
                    if ct2 = Error then (Error, bq) else
                    (parseTreeFromList [ct1; ct2], cc2)
                match bt with
                | Error -> (parseTreeFromList (List.rev list), bq)
                | _ -> z (bt :: list) bc
            z [] ac2
        if at3 = Error then (Error, q) else
        let (at4, ac4) = Parse.isText ")" ac3
        if at4 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3; at4], ac4)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("args", tree), next)

and assign (q:TokenCursor) =
    let result = // 'let' lexpr '=' expr
        let (at1, ac1) = Parse.isText "let" q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = lexpr ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = Parse.isText "=" ac2
        if at3 = Error then (Error, q) else
        let (at4, ac4) = expr ac3
        if at4 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3; at4], ac4)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("assign", tree), next)

and atom (q:TokenCursor) =
    let result = // NAT | STRING | OPERATOR | lambda | parens | ID | cases | ifthen
        let (at1, ac1) = Parse.isNat q
        if at1 <> Error then (at1, ac1) else
        let (at2, ac2) = Parse.isString q
        if at2 <> Error then (at2, ac2) else
        let (at3, ac3) = Parse.isOperator q
        if at3 <> Error then (at3, ac3) else
        let (at4, ac4) = lambda q
        if at4 <> Error then (at4, ac4) else
        let (at5, ac5) = parens q
        if at5 <> Error then (at5, ac5) else
        let (at6, ac6) = Parse.isId q
        if at6 <> Error then (at6, ac6) else
        let (at7, ac7) = cases q
        if at7 <> Error then (at7, ac7) else
        let (at8, ac8) = ifthen q
        if at8 <> Error then (at8, ac8) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("atom", tree), next)

and cases (q:TokenCursor) =
    let result = // 'case' expr (pattern '->' expr)+ ('otherwise' expr)?
        let (at1, ac1) = Parse.isText "case" q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = expr ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = // (pattern '->' expr)+
            let rec z list (bq:TokenCursor) =
                let (bt, bc) = // pattern '->' expr
                    let (ct1, cc1) = pattern bq
                    if ct1 = Error then (Error, bq) else
                    let (ct2, cc2) = Parse.isText "->" cc1
                    if ct2 = Error then (Error, bq) else
                    let (ct3, cc3) = expr cc2
                    if ct3 = Error then (Error, bq) else
                    (parseTreeFromList [ct1; ct2; ct3], cc3)
                match bt with
                | Error -> (parseTreeFromList (List.rev list), bq)
                | _ -> z (bt :: list) bc
            match z [] ac2 with
            | (Error, _) -> (Error, q)
            | (Empty, _) -> (Error, q)
            | (Node [], _) -> (Error, q)
            | (tree, next) -> (tree, next)
        if at3 = Error then (Error, q) else
        let (at4, ac4) = // ('otherwise' expr)?
            let (bt, bc) = // 'otherwise' expr
                let (ct1, cc1) = Parse.isText "otherwise" ac3
                if ct1 = Error then (Error, ac3) else
                let (ct2, cc2) = expr cc1
                if ct2 = Error then (Error, ac3) else
                (parseTreeFromList [ct1; ct2], cc2)
            match bt with
            | Error -> (Empty, ac3)
            | _ -> (bt, bc)
        if at4 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3; at4], ac4)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("cases", tree), next)

and expr (q:TokenCursor) =
    let result = // (assign | typeDecl)* atom args? SUPERSCRIPT?
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
        let (at3, ac3) = // args?
            let (bt, bc) = args ac2
            match bt with
            | Error -> (Empty, ac2)
            | _ -> (bt, bc)
        if at3 = Error then (Error, q) else
        let (at4, ac4) = // SUPERSCRIPT?
            let (bt, bc) = Parse.isSuperscript ac3
            match bt with
            | Error -> (Empty, ac3)
            | _ -> (bt, bc)
        if at4 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3; at4], ac4)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("expr", tree), next)

and ifthen (q:TokenCursor) =
    let result = // 'if' expr 'then' expr 'else' expr
        let (at1, ac1) = Parse.isText "if" q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = expr ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = Parse.isText "then" ac2
        if at3 = Error then (Error, q) else
        let (at4, ac4) = expr ac3
        if at4 = Error then (Error, q) else
        let (at5, ac5) = Parse.isText "else" ac4
        if at5 = Error then (Error, q) else
        let (at6, ac6) = expr ac5
        if at6 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3; at4; at5; at6], ac6)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("ifthen", tree), next)

and lambda (q:TokenCursor) =
    let result = // '(' lexpr (',' lexpr)* ')' '=' expr
        let (at1, ac1) = Parse.isText "(" q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = lexpr ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = // (',' lexpr)*
            let rec z list (bq:TokenCursor) =
                let (bt, bc) = // ',' lexpr
                    let (ct1, cc1) = Parse.isText "," bq
                    if ct1 = Error then (Error, bq) else
                    let (ct2, cc2) = lexpr cc1
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

and lexpr (q:TokenCursor) =
    let result = // ID ('(' lexpr (',' lexpr)* ')')?
        let (at1, ac1) = Parse.isId q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = // ('(' lexpr (',' lexpr)* ')')?
            let (bt, bc) = // '(' lexpr (',' lexpr)* ')'
                let (ct1, cc1) = Parse.isText "(" ac1
                if ct1 = Error then (Error, ac1) else
                let (ct2, cc2) = lexpr cc1
                if ct2 = Error then (Error, ac1) else
                let (ct3, cc3) = // (',' lexpr)*
                    let rec z list (dq:TokenCursor) =
                        let (dt, dc) = // ',' lexpr
                            let (et1, ec1) = Parse.isText "," dq
                            if et1 = Error then (Error, dq) else
                            let (et2, ec2) = lexpr ec1
                            if et2 = Error then (Error, dq) else
                            (parseTreeFromList [et1; et2], ec2)
                        match dt with
                        | Error -> (parseTreeFromList (List.rev list), dq)
                        | _ -> z (dt :: list) dc
                    z [] cc2
                if ct3 = Error then (Error, ac1) else
                let (ct4, cc4) = Parse.isText ")" cc3
                if ct4 = Error then (Error, ac1) else
                (parseTreeFromList [ct1; ct2; ct3; ct4], cc4)
            match bt with
            | Error -> (Empty, ac1)
            | _ -> (bt, bc)
        if at2 = Error then (Error, q) else
        (parseTreeFromList [at1; at2], ac2)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("lexpr", tree), next)

and monoType (q:TokenCursor) =
    let result = // lexpr (('→' | '->') lexpr)*
        let (at1, ac1) = lexpr q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = // (('→' | '->') lexpr)*
            let rec z list (bq:TokenCursor) =
                let (bt, bc) = // ('→' | '->') lexpr
                    let (ct1, cc1) = // '→' | '->'
                        let (dt1, dc1) = Parse.isText "→" bq
                        if dt1 <> Error then (dt1, dc1) else
                        let (dt2, dc2) = Parse.isText "->" bq
                        if dt2 <> Error then (dt2, dc2) else
                        (Error, bq)
                    if ct1 = Error then (Error, bq) else
                    let (ct2, cc2) = lexpr cc1
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

and parens (q:TokenCursor) =
    let result = // '(' expr ')'
        let (at1, ac1) = Parse.isText "(" q
        if at1 = Error then (Error, q) else
        let (at2, ac2) = expr ac1
        if at2 = Error then (Error, q) else
        let (at3, ac3) = Parse.isText ")" ac2
        if at3 = Error then (Error, q) else
        (parseTreeFromList [at1; at2; at3], ac3)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("parens", tree), next)

and pattern (q:TokenCursor) =
    let result = // ID ('(' pattern (',' pattern)* ')')? | NAT | STRING
        let (at1, ac1) = // ID ('(' pattern (',' pattern)* ')')?
            let (bt1, bc1) = Parse.isId q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = // ('(' pattern (',' pattern)* ')')?
                let (ct, cc) = // '(' pattern (',' pattern)* ')'
                    let (dt1, dc1) = Parse.isText "(" bc1
                    if dt1 = Error then (Error, bc1) else
                    let (dt2, dc2) = pattern dc1
                    if dt2 = Error then (Error, bc1) else
                    let (dt3, dc3) = // (',' pattern)*
                        let rec z list (eq:TokenCursor) =
                            let (et, ec) = // ',' pattern
                                let (ft1, fc1) = Parse.isText "," eq
                                if ft1 = Error then (Error, eq) else
                                let (ft2, fc2) = pattern fc1
                                if ft2 = Error then (Error, eq) else
                                (parseTreeFromList [ft1; ft2], fc2)
                            match et with
                            | Error -> (parseTreeFromList (List.rev list), eq)
                            | _ -> z (et :: list) ec
                        z [] dc2
                    if dt3 = Error then (Error, bc1) else
                    let (dt4, dc4) = Parse.isText ")" dc3
                    if dt4 = Error then (Error, bc1) else
                    (parseTreeFromList [dt1; dt2; dt3; dt4], dc4)
                match ct with
                | Error -> (Empty, bc1)
                | _ -> (ct, cc)
            if bt2 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2], bc2)
        if at1 <> Error then (at1, ac1) else
        let (at2, ac2) = Parse.isNat q
        if at2 <> Error then (at2, ac2) else
        let (at3, ac3) = Parse.isString q
        if at3 <> Error then (at3, ac3) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("pattern", tree), next)

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

