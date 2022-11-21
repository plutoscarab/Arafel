module Parser
// Generated code. Do not edit.

open Tokens
open Lexer
open Parse
open Syntax

let keywords = Set [
    "case"; "elif"; "else"; "fn"; "forall"; "if"; "let"; "of"; "then"; "type"; 
]

let rec parseAtom' () =
    parser {
        return! parser {
            let! f0 = parseExponent
            return ExponentA(f0)
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

and parseCase' () =
    parser {
        let! f0 = parsePattern
        let! f1 = andThen (literal ":") (parseExpr)
        return Case(f0, f1)
    }

and parseCases' () =
    parser {
        let! f0 = andThen (literal "case") (checkpoint (parseExpr))
        let! f1 = checkpoint (andThen (literal "of") (oneOrMore (parseCase)))
        let! f2 = option (andThen (literal "else") (checkpoint (parseExpr)))
        return Cases(f0, f1, f2)
    }

and parseElseIf' () =
    parser {
        let! f0 = andThen (literal "elif") (checkpoint (parseExpr))
        let! f1 = checkpoint (andThen (literal "then") (parseExpr))
        return ElseIf(f0, f1)
    }

and parseExponent' () =
    parser {
        let! f0 = parseExpr
        let! f1 = bigintToken Superscript "Superscript"
        return Exponent(f0, f1)
    }

and parseExpr' () =
    parser {
        let! f0 = parseAtom
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parseExpr)))
        return Expr(f0, f1)
    }

and parseIfThen' () =
    parser {
        let! f0 = andThen (literal "if") (checkpoint (parseExpr))
        let! f1 = checkpoint (andThen (literal "then") (parseExpr))
        let! f2 = zeroOrMore (parseElseIf)
        let! f3 = checkpoint (andThen (literal "else") (parseExpr))
        return IfThen(f0, f1, f2, f3)
    }

and parseLambda' () =
    parser {
        let! f0 = andThen (literal "fn") (surround (literal "(") (literal ")") (parseLexpr))
        let! f1 = andThen (literal "=") (checkpoint (parseExpr))
        return Lambda(f0, f1)
    }

and parseLetDecl' () =
    parser {
        let! f0 = andThen (literal "let") (checkpoint (parseLexpr))
        let! f1 = checkpoint (andThen (literal "=") (parseExpr))
        let! f2 = checkpoint (parseExpr)
        return LetDecl(f0, f1, f2)
    }

and parseLexpr' () =
    parser {
        let! f0 = parseLexprName
        let! f1 = optionlist (surround (literal "(") (literal ")") (delimited (literal ",") (parseLexpr)))
        return Lexpr(f0, f1)
    }

and parseLexprName' () =
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

and parseMonoType' () =
    parser {
        let! f0 = delimited (orElse (literal "->") (literal "→")) (parseLexpr)
        return MonoType(f0)
    }

and parsePattern' () =
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

and parsePolyType' () =
    parser {
        let! f0 = optionlist (surround (orElse (literal "forall") (literal "∀")) (checkpoint (literal ",")) (oneOrMore (stringToken Identifier "Identifier")))
        let! f1 = delimited (literal "|") (parseMonoType)
        return PolyType(f0, f1)
    }

and parseTypeDecl' () =
    parser {
        let! f0 = andThen (literal "type") (checkpoint (stringToken Identifier "Identifier"))
        let! f1 = optionlist (surround (literal "(") (literal ")") (checkpoint (delimited (literal ",") (stringToken Identifier "Identifier"))))
        let! f2 = checkpoint (andThen (literal "=") (parsePolyType))
        let! f3 = parseExpr
        return TypeDecl(f0, f1, f2, f3)
    }

and parseAtom tokens history =
    if recursion "Atom" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseAtom'() tokens (("Atom", tokenIndex tokens)::history)

and parseCase tokens history =
    if recursion "Case" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseCase'() tokens (("Case", tokenIndex tokens)::history)

and parseCases tokens history =
    if recursion "Cases" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseCases'() tokens (("Cases", tokenIndex tokens)::history)

and parseElseIf tokens history =
    if recursion "ElseIf" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseElseIf'() tokens (("ElseIf", tokenIndex tokens)::history)

and parseExponent tokens history =
    if recursion "Exponent" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseExponent'() tokens (("Exponent", tokenIndex tokens)::history)

and parseExpr tokens history =
    if recursion "Expr" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseExpr'() tokens (("Expr", tokenIndex tokens)::history)

and parseIfThen tokens history =
    if recursion "IfThen" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseIfThen'() tokens (("IfThen", tokenIndex tokens)::history)

and parseLambda tokens history =
    if recursion "Lambda" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseLambda'() tokens (("Lambda", tokenIndex tokens)::history)

and parseLetDecl tokens history =
    if recursion "LetDecl" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseLetDecl'() tokens (("LetDecl", tokenIndex tokens)::history)

and parseLexpr tokens history =
    if recursion "Lexpr" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseLexpr'() tokens (("Lexpr", tokenIndex tokens)::history)

and parseLexprName tokens history =
    if recursion "LexprName" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseLexprName'() tokens (("LexprName", tokenIndex tokens)::history)

and parseMonoType tokens history =
    if recursion "MonoType" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseMonoType'() tokens (("MonoType", tokenIndex tokens)::history)

and parsePattern tokens history =
    if recursion "Pattern" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parsePattern'() tokens (("Pattern", tokenIndex tokens)::history)

and parsePolyType tokens history =
    if recursion "PolyType" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parsePolyType'() tokens (("PolyType", tokenIndex tokens)::history)

and parseTypeDecl tokens history =
    if recursion "TypeDecl" tokens history then
        Nomatch ["non-recursion"], tokens
    else
        parseTypeDecl'() tokens (("TypeDecl", tokenIndex tokens)::history)
