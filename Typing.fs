module Typing

open System

let mutable private nextId : uint64 = (uint64)(Environment.TickCount &&& 0x7FFFFFFF)

let private memoize f =
    let mutable table = Map.empty<_, _>
    fun key ->
        match table.TryFind key with
        | Some v -> v
        | None ->
            let value = f key
            table <- Map.add key value table
            value

type TypeId =
    | TypeId of uint64

let private mkTypeId() =
    nextId <- nextId + 1UL
    TypeId (nextId * 0xc44bc9d3acd592ceUL)

type GenericType =
    | Generic of TypeId * int // arity

type FunctionType =
    | Function of TypeId * TypeExpr list

and TypeExpr =
    | NativeType of TypeId
    | TypeVar of TypeId
    | GenericType of GenericType
    | ConcreteType of TypeId * GenericType * TypeExpr list
    | FunctionType of FunctionType
    | ProductType of TypeId * TypeExpr list
    with
    static member (*)(a, b) =
        let mkProduct' (a, b) = ProductType (mkTypeId(), [a; b])
        (memoize mkProduct') (a, b)
    static member (-)(a, b) =
        let mkFunction' (a, b) = FunctionType (Function (mkTypeId(), [a; b]))
        (memoize mkFunction') (a, b)

let mkNative() =
    NativeType (mkTypeId())

let mkVar() =
    TypeVar (mkTypeId())

let mkGeneric arity =
    if arity < 1 then raise (ArgumentOutOfRangeException "arity")
    Generic (mkTypeId(), arity)

let mkConcrete (Generic (id, arity)) args =
    let mkConcrete' (generic, args) =
        if arity = List.length args then
            ConcreteType (mkTypeId(), generic, args)
        else
            raise (ArgumentException "args")
    (memoize mkConcrete') (Generic (id, arity), args)

let mkFunction =
    let mkFunction' args =
        FunctionType (Function (mkTypeId(), args))
    memoize mkFunction'

let unitType = mkNative()
let typeType = mkNative()
let boolType = mkNative()
let intType = mkNative()
let charType = mkNative()
let strType = mkNative()

let nativeTypes = Map [
    "unit", unitType;
    "type", typeType;
    "bool", boolType;
    "int", intType;
    "char", charType;
    "str", strType;
]

type Operation =
    | Operation of string * TypeExpr list

let nativeOps = Set [
    Operation ("-", [intType; intType]);
    Operation ("+", [intType; intType; intType]);
    Operation ("-", [intType; intType; intType]);
    Operation ("*", [intType; intType; intType]);
    Operation ("/", [intType; intType; intType]);
]
