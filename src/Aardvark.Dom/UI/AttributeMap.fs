namespace Aardvark.Dom

open FSharp.Data.Adaptive

type AttributeMap(content : amap<string, AttributeValue>) =
    member x.Content = content


module AttributeMap =

    module private HashMap =
        let ofAttributes (attributes : #seq<Attribute>) =
            let mutable res = HashMap.empty<string, AttributeValue>
            for att in attributes do
                res <- 
                    res |> HashMap.alter att.Name (function 
                        | Some o -> Attribute.Merge(Attribute(att.Name, o), att).Value |> Some
                        | None -> Some att.Value
                    )
            res

    let empty = AttributeMap AMap.empty

    let single (att : Attribute) =
        AttributeMap (AMap.single att.Name att.Value)

    let ofSeq (attributes : #seq<Attribute>) =
        let res = HashMap.ofAttributes attributes
        AttributeMap (AMap.ofHashMap res)

    let ofList (attributes : list<Attribute>) =
        let mutable res = HashMap.empty<string, AttributeValue>
        for att in attributes do
            res <- 
                res |> HashMap.alter att.Name (function 
                    | Some o -> Attribute.Merge(Attribute(att.Name, o), att).Value |> Some
                    | None -> Some att.Value
                )
        AttributeMap (AMap.ofHashMap res)

    let ofArray (attributes : Attribute[]) =
        let mutable res = HashMap.empty<string, AttributeValue>
        for att in attributes do
            res <- 
                res |> HashMap.alter att.Name (function 
                    | Some o -> Attribute.Merge(Attribute(att.Name, o), att).Value |> Some
                    | None -> Some att.Value
                )
        AttributeMap (AMap.ofHashMap res)

    let union (a : AttributeMap) (b : AttributeMap) =
        (a.Content, b.Content) 
        ||> AMap.unionWith (fun k a b -> Attribute.Merge(Attribute(k, a), Attribute(k, b)).Value)
        |> AttributeMap

    let ofAVal (attribute : aval<Attribute>) =
        attribute 
        |> AVal.map (fun a -> HashMap.single a.Name a.Value)
        |> AMap.ofAVal
        |> AttributeMap
        
    let ofSeqA (attribute : aval<#seq<Attribute>>) =
        attribute 
        |> AVal.map HashMap.ofAttributes
        |> AMap.ofAVal
        |> AttributeMap
        
    let ofOptionA (attribute : aval<option<Attribute>>) =
        attribute 
        |> AVal.map (function Some att -> HashMap.single att.Name att.Value | _ -> HashMap.empty)
        |> AMap.ofAVal
        |> AttributeMap
        
 



type ChangeableAttributeMap(content : cmap<string, AttributeValue>) =
    inherit AttributeMap(content)

    member x.Add(att : Attribute) =
        content.[att.Name] <- att.Value

    member x.Remove(att : Attribute) =
        content.Remove att.Name
