namespace Demo

open Adaptify
open FSharp.Data.Adaptive
open Aardvark.Dom

[<ModelType>]
type Model =
    {
        Count : int
        PressedButtons : HashSet<Button>
    }
