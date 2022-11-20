
open System
open System.Text

let eq a b =
    if a <> b then
        raise (Exception $"Expected {a} but got {b}")

let main =
    0