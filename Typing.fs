module Typing

open System

let mutable private nextId : int = 0

type TypeId =
    | TypeId of int

let private mkTypeId() =
    nextId <- nextId + 1
    TypeId nextId

type GenericType =
    | Generic of TypeId

type TypeExpr =
    | NativeType of TypeId
    | TypeVar of TypeId
    | ConcreteType of TypeId * GenericType * TypeExpr
    | FunctionType of TypeId * TypeExpr list
    | SumType of TypeId * TypeExpr list
    | ProductType of TypeId * TypeExpr list
    with
    static member (*)(a, b) = ProductType(mkTypeId(), [a; b])

let mkNative() =
    NativeType (mkTypeId())

let mkVar() =
    TypeVar (mkTypeId())

let mkGeneric() =
    Generic (mkTypeId())

let private memoize f =
    let mutable table = Map.empty<_, _>
    fun key ->
        match table.TryFind key with
        | Some v -> v
        | None ->
            let value = f key
            table <- Map.add key value table
            value

let mkConcrete generic arg =
    let mkConcrete' (generic, arg) =
        ConcreteType(mkTypeId(), generic, arg)
    (memoize mkConcrete') (generic, arg)

let mkFunction =
    let mkFunction' args =
        FunctionType(mkTypeId(), args)
    memoize mkFunction'

let mkProduct =
    let mkProduct' types =
        ProductType(mkTypeId(), types)
    memoize mkProduct'

let mkSum = 
    let mkSum' types =
        SumType(mkTypeId(), types)
    memoize mkSum'

let unitType = mkNative()
let typeType = mkNative()
let boolType = mkNative()
let intType = mkNative()
let charType = mkNative()
let strType = mkNative()

let nativeTypes = Map [
    "Unit", unitType;
    "Type", typeType;
    "Bool", boolType;
    "Int", intType;
    "Char", charType;
    "Str", strType;
]

let maybeType = mkGeneric()

let nothingType =
    mkConcrete maybeType (mkVar())

let justType =
    let var = mkVar()
    mkFunction [var; (mkConcrete maybeType var)]