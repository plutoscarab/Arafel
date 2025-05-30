module Parser
// Generated code. Do not edit.

open Tokens
open Lexer
open Parse
open Syntax

let keywords = Set [
    "af"; "by"; "case"; "elif"; "else"; "forall"; "if"; "in"; "let"; "of"; 
    "then"; "type"; 
]

let rec parseAtom' () =
    let baseParser =
        let p0 = fun t0 ->
            let (r1, t1) = (surround (literal "[") (literal "]") (parseIntSeq)) t0
            match r1 with
            | Match intSeq ->
                Match (IntSeqA(intSeq)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        
        let p1 = fun t0 ->
            let (r1, t1) = (bigintToken Nat "Nat") t0
            match r1 with
            | Match value ->
                Match (NatA(value)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        
        let p2 = fun t0 ->
            let (r1, t1) = (stringToken String "String") t0
            match r1 with
            | Match value ->
                Match (StringA(value)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        
        let p3 = fun t0 ->
            let (r1, t1) = (boolToken Bool "Bool") t0
            match r1 with
            | Match value ->
                Match (BoolA(value)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        
        let p4 = fun t0 ->
            let (r1, t1) = (surround (literal "[") (literal "]") (stringToken Operator "Operator")) t0
            match r1 with
            | Match symbol ->
                Match (OperatorA(symbol)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        
        let p5 = fun t0 ->
            let (r1, t1) = (surround (literal "(") (literal ")") (checkpoint (parseExpr))) t0
            match r1 with
            | Match expr ->
                Match (ParensA(expr)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        
        let p6 = fun t0 ->
            let (r1, t1) = (stringToken Identifier "Identifier") t0
            match r1 with
            | Match name ->
                Match (IdentifierA(name)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        
        fun t ->
            let mutable exp = []
            match p0 t with
            | Match r0, t2 -> Match r0, t2
            | SyntaxError e, t2 -> SyntaxError e, t2
            | Nomatch e0, _ ->
                exp <- e0 @ exp
                match p1 t with
                | Match r1, t2 -> Match r1, t2
                | SyntaxError e, t2 -> SyntaxError e, t2
                | Nomatch e1, _ ->
                    exp <- e1 @ exp
                    match p2 t with
                    | Match r2, t2 -> Match r2, t2
                    | SyntaxError e, t2 -> SyntaxError e, t2
                    | Nomatch e2, _ ->
                        exp <- e2 @ exp
                        match p3 t with
                        | Match r3, t2 -> Match r3, t2
                        | SyntaxError e, t2 -> SyntaxError e, t2
                        | Nomatch e3, _ ->
                            exp <- e3 @ exp
                            match p4 t with
                            | Match r4, t2 -> Match r4, t2
                            | SyntaxError e, t2 -> SyntaxError e, t2
                            | Nomatch e4, _ ->
                                exp <- e4 @ exp
                                match p5 t with
                                | Match r5, t2 -> Match r5, t2
                                | SyntaxError e, t2 -> SyntaxError e, t2
                                | Nomatch e5, _ ->
                                    exp <- e5 @ exp
                                    match p6 t with
                                    | Match r6, t2 -> Match r6, t2
                                    | SyntaxError e, t2 -> SyntaxError e, t2
                                    | Nomatch e6, _ ->
                                        exp <- e6 @ exp
                                        Nomatch exp, t
    
    let suffixes baseAtom =
        let p0 = fun t0 ->
            let (r1, t1) = (surround (literal "(") (literal ")") (delimited (literal ",") (parseExpr))) t0
            match r1 with
            | Match args ->
                Match (CallA(baseAtom, args)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        
        let p1 = fun t0 ->
            let (r1, t1) = (bigintToken Superscript "Superscript") t0
            match r1 with
            | Match exponent ->
                Match (ExponentA(baseAtom, exponent)), t1
            | SyntaxError e -> SyntaxError e, t1
            | Nomatch e -> Nomatch e, t0
        
        fun t ->
            let mutable exp = []
            match p0 t with
            | Match r0, t2 -> Match r0, t2
            | SyntaxError e, t2 -> SyntaxError e, t2
            | Nomatch e0, _ ->
                exp <- e0 @ exp
                match p1 t with
                | Match r1, t2 -> Match r1, t2
                | SyntaxError e, t2 -> SyntaxError e, t2
                | Nomatch e1, _ ->
                    exp <- e1 @ exp
                    Nomatch exp, t
    
    fun t ->
        match baseParser t with
        | Nomatch e, _ -> Nomatch e, t
        | SyntaxError e, t2 -> SyntaxError e, t2
        | Match baseAtom, t2 -> (repeat baseAtom suffixes) t2

and parseCase' () =
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
    let p0 = fun t0 ->
        let (r1, t1) = (parseUnatom) t0
        match r1 with
        | Match term ->
            let (r2, t2) = (zeroOrMore (parseTerm)) t1
            match r2 with
            | Match terms ->
                Match (Expr(term, terms)), t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    let p1 = fun t0 ->
        let (r1, t1) = (parseTypeDecl) t0
        match r1 with
        | Match typeDecl ->
            Match (TypeE(typeDecl)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    let p2 = fun t0 ->
        let (r1, t1) = (parseLetDecl) t0
        match r1 with
        | Match letDecl ->
            Match (LetE(letDecl)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    let p3 = fun t0 ->
        let (r1, t1) = (parseCases) t0
        match r1 with
        | Match cases ->
            Match (CasesE(cases)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    let p4 = fun t0 ->
        let (r1, t1) = (parseIfThen) t0
        match r1 with
        | Match ifthen ->
            Match (IfThenE(ifthen)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    let p5 = fun t0 ->
        let (r1, t1) = (andThen (literal "af") (surround (literal "(") (literal ")") (parseLexpr))) t0
        match r1 with
        | Match name ->
            let (r2, t2) = (andThen (literal "=") (checkpoint (parseExpr))) t1
            match r2 with
            | Match expr ->
                Match (LambdaE(name, expr)), t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    let p6 = fun t0 ->
        let (r1, t1) = (surround (literal "{") (literal "}") (parseExpr)) t0
        match r1 with
        | Match expr ->
            Match (CurlyLambdaE(expr)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    fun t ->
        let mutable exp = []
        match p0 t with
        | Match r0, t2 -> Match r0, t2
        | SyntaxError e, t2 -> SyntaxError e, t2
        | Nomatch e0, _ ->
            exp <- e0 @ exp
            match p1 t with
            | Match r1, t2 -> Match r1, t2
            | SyntaxError e, t2 -> SyntaxError e, t2
            | Nomatch e1, _ ->
                exp <- e1 @ exp
                match p2 t with
                | Match r2, t2 -> Match r2, t2
                | SyntaxError e, t2 -> SyntaxError e, t2
                | Nomatch e2, _ ->
                    exp <- e2 @ exp
                    match p3 t with
                    | Match r3, t2 -> Match r3, t2
                    | SyntaxError e, t2 -> SyntaxError e, t2
                    | Nomatch e3, _ ->
                        exp <- e3 @ exp
                        match p4 t with
                        | Match r4, t2 -> Match r4, t2
                        | SyntaxError e, t2 -> SyntaxError e, t2
                        | Nomatch e4, _ ->
                            exp <- e4 @ exp
                            match p5 t with
                            | Match r5, t2 -> Match r5, t2
                            | SyntaxError e, t2 -> SyntaxError e, t2
                            | Nomatch e5, _ ->
                                exp <- e5 @ exp
                                match p6 t with
                                | Match r6, t2 -> Match r6, t2
                                | SyntaxError e, t2 -> SyntaxError e, t2
                                | Nomatch e6, _ ->
                                    exp <- e6 @ exp
                                    Nomatch exp, t

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

and parseIntSeq' () =
    fun t0 ->
        let (r1, t1) = (optionlist (delimited (literal ",") (parseRange))) t0
        match r1 with
        | Match ranges ->
            Match (IntSeq(ranges)), t1
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
                let (r3, t3) = (checkpoint (option (andThen (literal "in") (parseExpr)))) t2
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
    let p0 = fun t0 ->
        let (r1, t1) = (stringToken Identifier "Identifier") t0
        match r1 with
        | Match name ->
            Match (IdentifierN(name)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    let p1 = fun t0 ->
        let (r1, t1) = (stringToken Operator "Operator") t0
        match r1 with
        | Match symbol ->
            Match (OperatorN(symbol)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    fun t ->
        let mutable exp = []
        match p0 t with
        | Match r0, t2 -> Match r0, t2
        | SyntaxError e, t2 -> SyntaxError e, t2
        | Nomatch e0, _ ->
            exp <- e0 @ exp
            match p1 t with
            | Match r1, t2 -> Match r1, t2
            | SyntaxError e, t2 -> SyntaxError e, t2
            | Nomatch e1, _ ->
                exp <- e1 @ exp
                Nomatch exp, t

and parseMonoType' () =
    fun t0 ->
        let (r1, t1) = (delimited (orElse (literal "->") (literal "→")) (parseLexpr)) t0
        match r1 with
        | Match types ->
            Match (MonoType(types)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parsePattern' () =
    let p0 = fun t0 ->
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
    
    let p1 = fun t0 ->
        let (r1, t1) = (bigintToken Nat "Nat") t0
        match r1 with
        | Match value ->
            Match (NatPat(value)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    let p2 = fun t0 ->
        let (r1, t1) = (stringToken String "String") t0
        match r1 with
        | Match value ->
            Match (StringPat(value)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    let p3 = fun t0 ->
        let (r1, t1) = (boolToken Bool "Bool") t0
        match r1 with
        | Match value ->
            Match (BoolPat(value)), t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0
    
    fun t ->
        let mutable exp = []
        match p0 t with
        | Match r0, t2 -> Match r0, t2
        | SyntaxError e, t2 -> SyntaxError e, t2
        | Nomatch e0, _ ->
            exp <- e0 @ exp
            match p1 t with
            | Match r1, t2 -> Match r1, t2
            | SyntaxError e, t2 -> SyntaxError e, t2
            | Nomatch e1, _ ->
                exp <- e1 @ exp
                match p2 t with
                | Match r2, t2 -> Match r2, t2
                | SyntaxError e, t2 -> SyntaxError e, t2
                | Nomatch e2, _ ->
                    exp <- e2 @ exp
                    match p3 t with
                    | Match r3, t2 -> Match r3, t2
                    | SyntaxError e, t2 -> SyntaxError e, t2
                    | Nomatch e3, _ ->
                        exp <- e3 @ exp
                        Nomatch exp, t

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

and parseRange' () =
    fun t0 ->
        let (r1, t1) = (bigintToken Nat "Nat") t0
        match r1 with
        | Match first ->
            let (r2, t2) = (andThen (literal "..") (option (bigintToken Nat "Nat"))) t1
            match r2 with
            | Match last ->
                let (r3, t3) = (option (andThen (literal "by") (bigintToken Nat "Nat"))) t2
                match r3 with
                | Match skip ->
                    Match (Range(first, last, skip)), t3
                | SyntaxError e -> SyntaxError e, t3
                | Nomatch e -> Nomatch e, t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseTerm' () =
    fun t0 ->
        let (r1, t1) = (stringToken Operator "Operator") t0
        match r1 with
        | Match operator ->
            let (r2, t2) = (parseUnatom) t1
            match r2 with
            | Match atom ->
                Match (Term(operator, atom)), t2
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

and parseUnatom' () =
    fun t0 ->
        let (r1, t1) = (option (stringToken Operator "Operator")) t0
        match r1 with
        | Match operator ->
            let (r2, t2) = (parseAtom) t1
            match r2 with
            | Match atom ->
                Match (Unatom(operator, atom)), t2
            | SyntaxError e -> SyntaxError e, t2
            | Nomatch e -> Nomatch e, t1
        | SyntaxError e -> SyntaxError e, t1
        | Nomatch e -> Nomatch e, t0

and parseAtom = parseAtom'()
and parseCase = parseCase'()
and parseCases = parseCases'()
and parseElseIf = parseElseIf'()
and parseExpr = parseExpr'()
and parseIfThen = parseIfThen'()
and parseIntSeq = parseIntSeq'()
and parseLetDecl = parseLetDecl'()
and parseLexpr = parseLexpr'()
and parseLexprName = parseLexprName'()
and parseMonoType = parseMonoType'()
and parsePattern = parsePattern'()
and parsePolyType = parsePolyType'()
and parseRange = parseRange'()
and parseTerm = parseTerm'()
and parseTypeDecl = parseTypeDecl'()
and parseUnatom = parseUnatom'()
