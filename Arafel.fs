module Arafel
// Generated code. Do not edit.
// Doing this instead of parser combinators for more straightforward debugging.

open Lexer
open Parse

let rec args (q:TokenCursor) =
    let result = // '(' expr (',' expr)* ')'
        let (at1, ac1) = // '(' expr (',' expr)* ')'
            let (bt1, bc1) = Parse.isText "(" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = expr bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = // (',' expr)*
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = // ',' expr
                        let (dt1, dc1) = // ',' expr
                            let (et1, ec1) = Parse.isText "," cq
                            if et1 = Error then (Error, cq) else
                            let (et2, ec2) = expr ec1
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
            (parseTreeFromList [bt1; bt2; bt3; bt4], bc4)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("args", tree), next)

and assign (q:TokenCursor) =
    let result = // 'let' lexpr '=' expr
        let (at1, ac1) = // 'let' lexpr '=' expr
            let (bt1, bc1) = Parse.isText "let" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = lexpr bc1
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
        let (at1, ac1) = // 'case' expr (pattern '->' expr)+ ('otherwise' expr)?
            let (bt1, bc1) = Parse.isText "case" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = expr bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = // (pattern '->' expr)+
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = // pattern '->' expr
                        let (dt1, dc1) = // pattern '->' expr
                            let (et1, ec1) = pattern cq
                            if et1 = Error then (Error, cq) else
                            let (et2, ec2) = Parse.isText "->" ec1
                            if et2 = Error then (Error, cq) else
                            let (et3, ec3) = expr ec2
                            if et3 = Error then (Error, cq) else
                            (parseTreeFromList [et1; et2; et3], ec3)
                        if dt1 <> Error then (dt1, dc1) else
                        (Error, cq)
                    match ct with
                    | Error -> (parseTreeFromList (List.rev list), cq)
                    | _ -> z (ct :: list) cc
                match z [] bc2 with
                | (Error, _) -> (Error, q)
                | (Empty, _) -> (Error, q)
                | (Node [], _) -> (Error, q)
                | (tree, next) -> (tree, next)
            if bt3 = Error then (Error, q) else
            let (bt4, bc4) = // ('otherwise' expr)?
                let (ct, cc) = // 'otherwise' expr
                    let (dt1, dc1) = // 'otherwise' expr
                        let (et1, ec1) = Parse.isText "otherwise" bc3
                        if et1 = Error then (Error, bc3) else
                        let (et2, ec2) = expr ec1
                        if et2 = Error then (Error, bc3) else
                        (parseTreeFromList [et1; et2], ec2)
                    if dt1 <> Error then (dt1, dc1) else
                    (Error, bc3)
                match ct with
                | Error -> (Empty, bc3)
                | _ -> (ct, cc)
            if bt4 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3; bt4], bc4)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("cases", tree), next)

and expr (q:TokenCursor) =
    let result = // (assign | typeDecl)* atom args? SUPERSCRIPT?
        let (at1, ac1) = // (assign | typeDecl)* atom args? SUPERSCRIPT?
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
            let (bt3, bc3) = // args?
                let (ct, cc) = args bc2
                match ct with
                | Error -> (Empty, bc2)
                | _ -> (ct, cc)
            if bt3 = Error then (Error, q) else
            let (bt4, bc4) = // SUPERSCRIPT?
                let (ct, cc) = Parse.isSuperscript bc3
                match ct with
                | Error -> (Empty, bc3)
                | _ -> (ct, cc)
            if bt4 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3; bt4], bc4)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("expr", tree), next)

and ifthen (q:TokenCursor) =
    let result = // 'if' expr 'then' expr 'else' expr
        let (at1, ac1) = // 'if' expr 'then' expr 'else' expr
            let (bt1, bc1) = Parse.isText "if" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = expr bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = Parse.isText "then" bc2
            if bt3 = Error then (Error, q) else
            let (bt4, bc4) = expr bc3
            if bt4 = Error then (Error, q) else
            let (bt5, bc5) = Parse.isText "else" bc4
            if bt5 = Error then (Error, q) else
            let (bt6, bc6) = expr bc5
            if bt6 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3; bt4; bt5; bt6], bc6)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("ifthen", tree), next)

and lambda (q:TokenCursor) =
    let result = // '(' lexpr (',' lexpr)* ')' '=' expr
        let (at1, ac1) = // '(' lexpr (',' lexpr)* ')' '=' expr
            let (bt1, bc1) = Parse.isText "(" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = lexpr bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = // (',' lexpr)*
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = // ',' lexpr
                        let (dt1, dc1) = // ',' lexpr
                            let (et1, ec1) = Parse.isText "," cq
                            if et1 = Error then (Error, cq) else
                            let (et2, ec2) = lexpr ec1
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

and lexpr (q:TokenCursor) =
    let result = // (ID | OPERATOR) ('(' lexpr (',' lexpr)* ')')?
        let (at1, ac1) = // (ID | OPERATOR) ('(' lexpr (',' lexpr)* ')')?
            let (bt1, bc1) = // ID | OPERATOR
                let (ct1, cc1) = Parse.isId q
                if ct1 <> Error then (ct1, cc1) else
                let (ct2, cc2) = Parse.isOperator q
                if ct2 <> Error then (ct2, cc2) else
                (Error, q)
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = // ('(' lexpr (',' lexpr)* ')')?
                let (ct, cc) = // '(' lexpr (',' lexpr)* ')'
                    let (dt1, dc1) = // '(' lexpr (',' lexpr)* ')'
                        let (et1, ec1) = Parse.isText "(" bc1
                        if et1 = Error then (Error, bc1) else
                        let (et2, ec2) = lexpr ec1
                        if et2 = Error then (Error, bc1) else
                        let (et3, ec3) = // (',' lexpr)*
                            let rec z list (fq:TokenCursor) =
                                let (ft, fc) = // ',' lexpr
                                    let (gt1, gc1) = // ',' lexpr
                                        let (ht1, hc1) = Parse.isText "," fq
                                        if ht1 = Error then (Error, fq) else
                                        let (ht2, hc2) = lexpr hc1
                                        if ht2 = Error then (Error, fq) else
                                        (parseTreeFromList [ht1; ht2], hc2)
                                    if gt1 <> Error then (gt1, gc1) else
                                    (Error, fq)
                                match ft with
                                | Error -> (parseTreeFromList (List.rev list), fq)
                                | _ -> z (ft :: list) fc
                            z [] ec2
                        if et3 = Error then (Error, bc1) else
                        let (et4, ec4) = Parse.isText ")" ec3
                        if et4 = Error then (Error, bc1) else
                        (parseTreeFromList [et1; et2; et3; et4], ec4)
                    if dt1 <> Error then (dt1, dc1) else
                    (Error, bc1)
                match ct with
                | Error -> (Empty, bc1)
                | _ -> (ct, cc)
            if bt2 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2], bc2)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("lexpr", tree), next)

and monoType (q:TokenCursor) =
    let result = // lexpr (('→' | '->') lexpr)*
        let (at1, ac1) = // lexpr (('→' | '->') lexpr)*
            let (bt1, bc1) = lexpr q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = // (('→' | '->') lexpr)*
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = // ('→' | '->') lexpr
                        let (dt1, dc1) = // ('→' | '->') lexpr
                            let (et1, ec1) = // '→' | '->'
                                let (ft1, fc1) = Parse.isText "→" cq
                                if ft1 <> Error then (ft1, fc1) else
                                let (ft2, fc2) = Parse.isText "->" cq
                                if ft2 <> Error then (ft2, fc2) else
                                (Error, cq)
                            if et1 = Error then (Error, cq) else
                            let (et2, ec2) = lexpr ec1
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

and parens (q:TokenCursor) =
    let result = // '(' expr ')'
        let (at1, ac1) = // '(' expr ')'
            let (bt1, bc1) = Parse.isText "(" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = expr bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = Parse.isText ")" bc2
            if bt3 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3], bc3)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
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
                    let (dt1, dc1) = // '(' pattern (',' pattern)* ')'
                        let (et1, ec1) = Parse.isText "(" bc1
                        if et1 = Error then (Error, bc1) else
                        let (et2, ec2) = pattern ec1
                        if et2 = Error then (Error, bc1) else
                        let (et3, ec3) = // (',' pattern)*
                            let rec z list (fq:TokenCursor) =
                                let (ft, fc) = // ',' pattern
                                    let (gt1, gc1) = // ',' pattern
                                        let (ht1, hc1) = Parse.isText "," fq
                                        if ht1 = Error then (Error, fq) else
                                        let (ht2, hc2) = pattern hc1
                                        if ht2 = Error then (Error, fq) else
                                        (parseTreeFromList [ht1; ht2], hc2)
                                    if gt1 <> Error then (gt1, gc1) else
                                    (Error, fq)
                                match ft with
                                | Error -> (parseTreeFromList (List.rev list), fq)
                                | _ -> z (ft :: list) fc
                            z [] ec2
                        if et3 = Error then (Error, bc1) else
                        let (et4, ec4) = Parse.isText ")" ec3
                        if et4 = Error then (Error, bc1) else
                        (parseTreeFromList [et1; et2; et3; et4], ec4)
                    if dt1 <> Error then (dt1, dc1) else
                    (Error, bc1)
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
    let result = // 'type' ID ('(' ID (',' ID)* ')')? '=' tyƿe
        let (at1, ac1) = // 'type' ID ('(' ID (',' ID)* ')')? '=' tyƿe
            let (bt1, bc1) = Parse.isText "type" q
            if bt1 = Error then (Error, q) else
            let (bt2, bc2) = Parse.isId bc1
            if bt2 = Error then (Error, q) else
            let (bt3, bc3) = // ('(' ID (',' ID)* ')')?
                let (ct, cc) = // '(' ID (',' ID)* ')'
                    let (dt1, dc1) = // '(' ID (',' ID)* ')'
                        let (et1, ec1) = Parse.isText "(" bc2
                        if et1 = Error then (Error, bc2) else
                        let (et2, ec2) = Parse.isId ec1
                        if et2 = Error then (Error, bc2) else
                        let (et3, ec3) = // (',' ID)*
                            let rec z list (fq:TokenCursor) =
                                let (ft, fc) = // ',' ID
                                    let (gt1, gc1) = // ',' ID
                                        let (ht1, hc1) = Parse.isText "," fq
                                        if ht1 = Error then (Error, fq) else
                                        let (ht2, hc2) = Parse.isId hc1
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
            let (bt4, bc4) = Parse.isText "=" bc3
            if bt4 = Error then (Error, q) else
            let (bt5, bc5) = tyƿe bc4
            if bt5 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3; bt4; bt5], bc5)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("typeDecl", tree), next)

and tyƿe (q:TokenCursor) =
    let result = // (('∀' | 'forall') ID+ ',')? monoType ('|' monoType)*
        let (at1, ac1) = // (('∀' | 'forall') ID+ ',')? monoType ('|' monoType)*
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
            let (bt3, bc3) = // ('|' monoType)*
                let rec z list (cq:TokenCursor) =
                    let (ct, cc) = // '|' monoType
                        let (dt1, dc1) = // '|' monoType
                            let (et1, ec1) = Parse.isText "|" cq
                            if et1 = Error then (Error, cq) else
                            let (et2, ec2) = monoType ec1
                            if et2 = Error then (Error, cq) else
                            (parseTreeFromList [et1; et2], ec2)
                        if dt1 <> Error then (dt1, dc1) else
                        (Error, cq)
                    match ct with
                    | Error -> (parseTreeFromList (List.rev list), cq)
                    | _ -> z (ct :: list) cc
                z [] bc2
            if bt3 = Error then (Error, q) else
            (parseTreeFromList [bt1; bt2; bt3], bc3)
        if at1 <> Error then (at1, ac1) else
        (Error, q)
    match result with
    | (Error, _) -> (Error, q)
    | (tree, next) -> (Production ("tyƿe", tree), next)

