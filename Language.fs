module Language
// Generated code. Do not edit. Make changes in language.txt.

open Lexer
open Parse

type Assign = 
    Assign of Lexpr * Expr

and Atom = 
    | NatA of bigint
    | StrA of string
    | OpA of string
    | LambdaA of Lambda
    | ParensA of Expr
    | IdA of string
    | MatchA of Matches
    | IfThenA of IfThen

and Case = 
    Case of Pattern * Expr

and Expr = 
    Expr of Prelude list * Atom * Expr list * Postfix list

and IfThen = 
    IfThen of Expr * Expr * Expr

and Lambda = 
    Lambda of Pattern * Expr

and Lexpr = 
    Lexpr of LexprName * Lexpr list

and LexprName = 
    | IdN of string
    | OpN of string

and Matches = 
    Matches of Expr * Case list * Expr option

and MonoType = 
    MonoType of Lexpr list

and Pattern = 
    | CtorPat of string * Pattern list
    | NatPat of bigint
    | StrPat of string

and Postfix = 
    | Super of bigint

and Prelude = 
    | AssignP of Assign
    | TypeDeclP of TypeDecl

and TypeDecl = 
    TypeDecl of string * string list * Tyƿe

and Tyƿe = 
    Tyƿe of string list * MonoType list

let rec assign () =
    parser {
        let! f0 =
            P (fun c -> (None, c))
        let! f1 =
            P (fun c -> (None, c))
        return f0, f1
    }

and atom () =
    let f0 () =
        P (fun c -> (None, c))
    let f1 () =
        P (fun c -> (None, c))
    let f2 () =
        P (fun c -> (None, c))
    let f3 () =
        P (fun c -> (None, c))
    let f4 () =
        P (fun c -> (None, c))
    let f5 () =
        P (fun c -> (None, c))
    let f6 () =
        P (fun c -> (None, c))
    let f7 () =
        P (fun c -> (None, c))
    parser {
        let! r = f0()
        return (NatA r)
    }
    <|> parser {
        let! r = f1()
        return (StrA r)
    }
    <|> parser {
        let! r = f2()
        return (OpA r)
    }
    <|> parser {
        let! r = f3()
        return (LambdaA r)
    }
    <|> parser {
        let! r = f4()
        return (ParensA r)
    }
    <|> parser {
        let! r = f5()
        return (IdA r)
    }
    <|> parser {
        let! r = f6()
        return (MatchA r)
    }
    <|> parser {
        let! r = f7()
        return (IfThenA r)
    }

and case () =
    parser {
        let! f0 =
            P (fun c -> (None, c))
        let! f1 =
            P (fun c -> (None, c))
        return f0, f1
    }

and expr () =
    parser {
        let! f0 =
            P (fun c -> (None, c))
        let! f1 =
            P (fun c -> (None, c))
        let! f2 =
            P (fun c -> (None, c))
        let! f3 =
            P (fun c -> (None, c))
        return f0, f1, f2, f3
    }

and ifThen () =
    parser {
        let! f0 =
            P (fun c -> (None, c))
        let! f1 =
            P (fun c -> (None, c))
        let! f2 =
            P (fun c -> (None, c))
        return f0, f1, f2
    }

and lambda () =
    parser {
        let! f0 =
            P (fun c -> (None, c))
        let! f1 =
            P (fun c -> (None, c))
        return f0, f1
    }

and lexpr () =
    parser {
        let! f0 =
            P (fun c -> (None, c))
        let! f1 =
            P (fun c -> (None, c))
        return f0, f1
    }

and lexprName () =
    let f0 () =
        P (fun c -> (None, c))
    let f1 () =
        P (fun c -> (None, c))
    parser {
        let! r = f0()
        return (IdN r)
    }
    <|> parser {
        let! r = f1()
        return (OpN r)
    }

and matches () =
    parser {
        let! f0 =
            P (fun c -> (None, c))
        let! f1 =
            P (fun c -> (None, c))
        let! f2 =
            P (fun c -> (None, c))
        return f0, f1, f2
    }

and monoType () =
    P (fun c -> (None, c))

and pattern () =
    let f0 () =
        parser {
            let! f0 =
                P (fun c -> (None, c))
            let! f1 =
                P (fun c -> (None, c))
            return f0, f1
        }
    let f1 () =
        P (fun c -> (None, c))
    let f2 () =
        P (fun c -> (None, c))
    parser {
        let! r = f0()
        return (CtorPat r)
    }
    <|> parser {
        let! r = f1()
        return (NatPat r)
    }
    <|> parser {
        let! r = f2()
        return (StrPat r)
    }

and postfix () =
    let f0 () =
        P (fun c -> (None, c))
    parser {
        let! r = f0()
        return (Super r)
    }

and prelude () =
    let f0 () =
        P (fun c -> (None, c))
    let f1 () =
        P (fun c -> (None, c))
    parser {
        let! r = f0()
        return (AssignP r)
    }
    <|> parser {
        let! r = f1()
        return (TypeDeclP r)
    }

and typeDecl () =
    parser {
        let! f0 =
            P (fun c -> (None, c))
        let! f1 =
            P (fun c -> (None, c))
        let! f2 =
            P (fun c -> (None, c))
        return f0, f1, f2
    }

and tyƿe () =
    parser {
        let! f0 =
            P (fun c -> (None, c))
        let! f1 =
            P (fun c -> (None, c))
        return f0, f1
    }

