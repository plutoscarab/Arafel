module Parser
// Generated code. Do not edit.

open Tokens
open Lexer
open Parse
open Syntax

let keywords = Set [
    "case"; "elif"; "else"; "fn"; "forall"; "if"; "let"; "of"; "then"; "type"; 
]

let rec parseCase' () =
    fun t0 ->
        let (r1, t1) = (parsePattern) t0
        match r1 with
        | Match pattern ->
            let (r2, t2) = (andThen (literal ":") (parseExpr)) t1
            match r2 with
            | Match expr ->
                Match (Case(pattern, expr)), t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseCases' () =
    fun t0 ->
        let (r1, t1) = (andThen (literal "case") (checkpoint (parseExpr))) t0
        match r1 with
        | Match expr ->
            let (r2, t2) = (checkpoint (andThen (literal "of") (oneOrMore (parseCase)))) t1
            match r2 with
            | Match cases ->
                let (r3, t3) = (option (andThen (literal "else") (checkpoint (parseExpr)))) t2
                match r3 with
                | Match otherwise ->
                    Match (Cases(expr, cases, otherwise)), t3
                | SyntaxError e -> SyntaxError e, t3
                | Nomatch e -> Nomatch e, t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseElseIf' () =
    fun t0 ->
        let (r1, t1) = (andThen (literal "elif") (checkpoint (parseExpr))) t0
        match r1 with
        | Match condition ->
            let (r2, t2) = (checkpoint (andThen (literal "then") (parseExpr))) t1
            match r2 with
            | Match trueExpr ->
                Match (ElseIf(condition, trueExpr)), t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseExpr' () =
    let baseParser = parser {
        return! fun t0 ->
            let (r1, t1) = (bigintToken Nat "Nat") t0
            match r1 with
            | Match value ->
                Match (NatE(value)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (stringToken String "String") t0
            match r1 with
            | Match value ->
                Match (StringE(value)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (stringToken Operator "Operator") t0
            match r1 with
            | Match symbol ->
                Match (OperatorE(symbol)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (parseLambda) t0
            match r1 with
            | Match lambda ->
                Match (LambdaE(lambda)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (surround (literal "(") (literal ")") (checkpoint (parseExpr))) t0
            match r1 with
            | Match expr ->
                Match (ParensE(expr)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (stringToken Identifier "Identifier") t0
            match r1 with
            | Match name ->
                Match (IdentifierE(name)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (parseCases) t0
            match r1 with
            | Match cases ->
                Match (CasesE(cases)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (parseIfThen) t0
            match r1 with
            | Match ifthen ->
                Match (IfThenE(ifthen)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (parseLetDecl) t0
            match r1 with
            | Match letDecl ->
                Match (LetE(letDecl)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (parseTypeDecl) t0
            match r1 with
            | Match typeDecl ->
                Match (TypeE(typeDecl)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
    }
    let suffixes baseExpr = parser {
        return! fun t0 ->
            let (r1, t1) = (surround (literal "(") (literal ")") (delimited (literal ",") (parseExpr))) t0
            match r1 with
            | Match args ->
                Match (CallE(baseExpr, args)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (bigintToken Superscript "Superscript") t0
            match r1 with
            | Match exponent ->
                Match (ExponentE(baseExpr, exponent)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
    }
    parser {
        let! baseExpr = baseParser
        return! repeat baseExpr suffixes
    }

and parseIfThen' () =
    fun t0 ->
        let (r1, t1) = (andThen (literal "if") (checkpoint (parseExpr))) t0
        match r1 with
        | Match condition ->
            let (r2, t2) = (checkpoint (andThen (literal "then") (parseExpr))) t1
            match r2 with
            | Match trueExpr ->
                let (r3, t3) = (zeroOrMore (parseElseIf)) t2
                match r3 with
                | Match elseifs ->
                    let (r4, t4) = (checkpoint (andThen (literal "else") (parseExpr))) t3
                    match r4 with
                    | Match falseExpr ->
                        Match (IfThen(condition, trueExpr, elseifs, falseExpr)), t4
                    | SyntaxError e -> SyntaxError e, t4
                    | Nomatch e -> Nomatch e, t3
                | SyntaxError e -> SyntaxError e, t3
                | Nomatch e -> Nomatch e, t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseLambda' () =
    fun t0 ->
        let (r1, t1) = (andThen (literal "fn") (surround (literal "(") (literal ")") (parseLexpr))) t0
        match r1 with
        | Match name ->
            let (r2, t2) = (andThen (literal "=") (checkpoint (parseExpr))) t1
            match r2 with
            | Match expr ->
                Match (Lambda(name, expr)), t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseLetDecl' () =
    fun t0 ->
        let (r1, t1) = (andThen (literal "let") (checkpoint (parseLexpr))) t0
        match r1 with
        | Match name ->
            let (r2, t2) = (checkpoint (andThen (literal "=") (parseExpr))) t1
            match r2 with
            | Match expr ->
                let (r3, t3) = (checkpoint (parseExpr)) t2
                match r3 with
                | Match inExpr ->
                    Match (LetDecl(name, expr, inExpr)), t3
                | SyntaxError e -> SyntaxError e, t3
                | Nomatch e -> Nomatch e, t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseLexpr' () =
    fun t0 ->
        let (r1, t1) = (parseLexprName) t0
        match r1 with
        | Match name ->
            let (r2, t2) = (optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parseLexpr)))) t1
            match r2 with
            | Match parameters ->
                Match (Lexpr(name, parameters)), t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseLexprName' () =
    parser {
        return! fun t0 ->
            let (r1, t1) = (stringToken Identifier "Identifier") t0
            match r1 with
            | Match name ->
                Match (IdentifierN(name)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (stringToken Operator "Operator") t0
            match r1 with
            | Match symbol ->
                Match (OperatorN(symbol)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
    }

and parseMonoType' () =
    fun t0 ->
        let (r1, t1) = (delimited (orElse (literal "->") (literal "→")) (parseLexpr)) t0
        match r1 with
        | Match types ->
            Match (MonoType(types)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parsePattern' () =
    parser {
        return! fun t0 ->
            let (r1, t1) = (stringToken Identifier "Identifier") t0
            match r1 with
            | Match ctor ->
                let (r2, t2) = (optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parsePattern)))) t1
                match r2 with
                | Match args ->
                    Match (CtorPat(ctor, args)), t2
                | SyntaxError e -> SyntaxError e, t2
                | Nomatch e -> Nomatch e, t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (bigintToken Nat "Nat") t0
            match r1 with
            | Match value ->
                Match (NatPat(value)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (stringToken String "String") t0
            match r1 with
            | Match value ->
                Match (StringPat(value)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        return! fun t0 ->
            let (r1, t1) = (boolToken Bool "Bool") t0
            match r1 with
            | Match value ->
                Match (BoolPat(value)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
    }

and parsePolyType' () =
    fun t0 ->
        let (r1, t1) = (optionlist (surround (orElse (literal "forall") (literal "∀")) (checkpoint (literal ",")) (oneOrMore (stringToken Identifier "Identifier")))) t0
        match r1 with
        | Match foralls ->
            let (r2, t2) = (delimited (literal "|") (parseMonoType)) t1
            match r2 with
            | Match cases ->
                Match (PolyType(foralls, cases)), t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseTypeDecl' () =
    fun t0 ->
        let (r1, t1) = (andThen (literal "type") (checkpoint (stringToken Identifier "Identifier"))) t0
        match r1 with
        | Match name ->
            let (r2, t2) = (optionlist (surround (literal "(") (literal ")") (checkpoint (delimited (literal ",") (stringToken Identifier "Identifier"))))) t1
            match r2 with
            | Match parameters ->
                let (r3, t3) = (checkpoint (andThen (literal "=") (parsePolyType))) t2
                match r3 with
                | Match ptype ->
                    let (r4, t4) = (parseExpr) t3
                    match r4 with
                    | Match inExpr ->
                        Match (TypeDecl(name, parameters, ptype, inExpr)), t4
                    | SyntaxError e -> SyntaxError e, t4
                    | Nomatch e -> Nomatch e, t3
                | SyntaxError e -> SyntaxError e, t3
                | Nomatch e -> Nomatch e, t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseCase = parseCase'()
and parseCases = parseCases'()
and parseElseIf = parseElseIf'()
and parseExpr = parseExpr'()
and parseIfThen = parseIfThen'()
and parseLambda = parseLambda'()
and parseLetDecl = parseLetDecl'()
and parseLexpr = parseLexpr'()
and parseLexprName = parseLexprName'()
and parseMonoType = parseMonoType'()
and parsePattern = parsePattern'()
and parsePolyType = parsePolyType'()
and parseTypeDecl = parseTypeDecl'()
