module Parser
// Generated code. Do not edit.

open Tokens
open Lexer
open Parse
open Syntax

let keywords = Set [
    "case"; "elif"; "else"; "fn"; "forall"; "if"; "let"; "of"; "then"; "type"; 
]

let rec parseAtom' toAvoid =
    parser {
        return! parser {
            if toAvoid <> 0 then
                let! f0 = parseAtom' 0
                return! parser {
                    let! f1 = bigintToken Superscript "Superscript"
                    return ExponentA(f0, f1)
                }
                return f0
        }
        return! parser {
            let! f0 = bigintToken Nat "Nat"
            return NatA(f0)
        }
        return! parser {
            let! f0 = stringToken String "String"
            return StringA(f0)
        }
        return! parser {
            let! f0 = stringToken Operator "Operator"
            return OperatorA(f0)
        }
        return! parser {
            let! f0 = parseLambda
            return LambdaA(f0)
        }
        return! parser {
            let! f0 = surround (literal "(") (literal ")") (checkpoint (parseExpr))
            return ParensA(f0)
        }
        return! parser {
            let! f0 = stringToken Identifier "Identifier"
            return IdentifierA(f0)
        }
        return! parser {
            let! f0 = parseCases
            return CasesA(f0)
        }
        return! parser {
            let! f0 = parseIfThen
            return IfThenA(f0)
        }
        return! parser {
            let! f0 = parseLetDecl
            return LetA(f0)
        }
        return! parser {
            let! f0 = parseTypeDecl
            return TypeA(f0)
        }
    }

and parseCase' toAvoid =
    parser {
        let! f0 = parsePattern
        let! f1 = andThen (literal ":") (parseExpr)
        return Case(f0, f1)
    }

and parseCases' toAvoid =
    parser {
        let! f0 = andThen (literal "case") (checkpoint (parseExpr))
        let! f1 = checkpoint (andThen (literal "of") (oneOrMore (parseCase)))
        let! f2 = option (andThen (literal "else") (checkpoint (parseExpr)))
        return Cases(f0, f1, f2)
    }

and parseElseIf' toAvoid =
    parser {
        let! f0 = andThen (literal "elif") (checkpoint (parseExpr))
        let! f1 = checkpoint (andThen (literal "then") (parseExpr))
        return ElseIf(f0, f1)
    }

and parseExpr' toAvoid =
    parser {
        let! f0 = parseAtom
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parseExpr)))
        return Expr(f0, f1)
    }

and parseIfThen' toAvoid =
    parser {
        let! f0 = andThen (literal "if") (checkpoint (parseExpr))
        let! f1 = checkpoint (andThen (literal "then") (parseExpr))
        let! f2 = zeroOrMore (parseElseIf)
        let! f3 = checkpoint (andThen (literal "else") (parseExpr))
        return IfThen(f0, f1, f2, f3)
    }

and parseLambda' toAvoid =
    parser {
        let! f0 = andThen (literal "fn") (surround (literal "(") (literal ")") (parseLexpr))
        let! f1 = andThen (literal "=") (checkpoint (parseExpr))
        return Lambda(f0, f1)
    }

and parseLetDecl' toAvoid =
    parser {
        let! f0 = andThen (literal "let") (checkpoint (parseLexpr))
        let! f1 = checkpoint (andThen (literal "=") (parseExpr))
        let! f2 = checkpoint (parseExpr)
        return LetDecl(f0, f1, f2)
    }

and parseLexpr' toAvoid =
    parser {
        let! f0 = parseLexprName
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parseLexpr)))
        return Lexpr(f0, f1)
    }

and parseLexprName' toAvoid =
    parser {
        return! parser {
            let! f0 = stringToken Identifier "Identifier"
            return IdentifierN(f0)
        }
        return! parser {
            let! f0 = stringToken Operator "Operator"
            return OperatorN(f0)
        }
    }

and parseMonoType' toAvoid =
    parser {
        let! f0 = delimited (orElse (literal "->") (literal "→")) (parseLexpr)
        return MonoType(f0)
    }

and parsePattern' toAvoid =
    parser {
        return! parser {
            let! f0 = stringToken Identifier "Identifier"
            let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parsePattern)))
            return CtorPat(f0, f1)
        }
        return! parser {
            let! f0 = bigintToken Nat "Nat"
            return NatPat(f0)
        }
        return! parser {
            let! f0 = stringToken String "String"
            return StringPat(f0)
        }
        return! parser {
            let! f0 = boolToken Bool "Bool"
            return BoolPat(f0)
        }
    }

and parsePolyType' toAvoid =
    parser {
        let! f0 = optionlist (surround (orElse (literal "forall") (literal "∀")) (checkpoint (literal ",")) (oneOrMore (stringToken Identifier "Identifier")))
        let! f1 = delimited (literal "|") (parseMonoType)
        return PolyType(f0, f1)
    }

and parseTypeDecl' toAvoid =
    parser {
        let! f0 = andThen (literal "type") (checkpoint (stringToken Identifier "Identifier"))
        let! f1 = optionlist (surround (literal "(") (literal ")") (checkpoint (delimited (literal ",") (stringToken Identifier "Identifier"))))
        let! f2 = checkpoint (andThen (literal "=") (parsePolyType))
        let! f3 = parseExpr
        return TypeDecl(f0, f1, f2, f3)
    }

and parseAtom t = (parseAtom' -1) t
and parseCase t = (parseCase' -1) t
and parseCases t = (parseCases' -1) t
and parseElseIf t = (parseElseIf' -1) t
and parseExpr t = (parseExpr' -1) t
and parseIfThen t = (parseIfThen' -1) t
and parseLambda t = (parseLambda' -1) t
and parseLetDecl t = (parseLetDecl' -1) t
and parseLexpr t = (parseLexpr' -1) t
and parseLexprName t = (parseLexprName' -1) t
and parseMonoType t = (parseMonoType' -1) t
and parsePattern t = (parsePattern' -1) t
and parsePolyType t = (parsePolyType' -1) t
and parseTypeDecl t = (parseTypeDecl' -1) t
