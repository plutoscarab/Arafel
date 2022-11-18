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
    with
    override this.ToString() =
        match this with
        | Function (id, args) ->
            let argstrs = List.map (fun a -> a.ToString()) args
            String.concat " -> " argstrs

and TypeExpr =
    | NativeType of TypeId * string
    | TypeVar of TypeId
    | GenericType of GenericType
    | ConcreteType of TypeId * GenericType * TypeExpr list
    | FunctionType of FunctionType
    | ProductType of TypeId * TypeExpr list
    with
    override this.ToString() =
        match this with
        | NativeType (_, name) -> name
        | TypeVar id -> $"TypeVar {id}"
        | GenericType g -> $"GenericType {g}"
        | ConcreteType (id, g, args) ->
            let argstrs = List.map (fun a -> a.ToString()) args
            $"{g}(" + (String.concat ", " argstrs) + ")"
        | FunctionType f -> f.ToString()
        | ProductType (id, args) ->
            let argstrs = List.map (fun a -> a.ToString()) args
            String.concat " * " argstrs
    static member (*)(a, b) =
        let mkProduct' (a, b) = ProductType (mkTypeId(), [a; b])
        (memoize mkProduct') (a, b)
    static member (-)(a, b) =
        let mkFunction' (a, b) = FunctionType (Function (mkTypeId(), [a; b]))
        (memoize mkFunction') (a, b)

let mkNative name =
    NativeType (mkTypeId(), name)

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

let unitType = mkNative "unit"
let typeType = mkNative "type"
let boolType = mkNative "bool"
let intType = mkNative "int"
let charType = mkNative "char"
let strType = mkNative "str"

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
