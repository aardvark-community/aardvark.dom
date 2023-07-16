namespace Aardvark.Dom.Bootstrap

open FSharp.Data.Adaptive
open Aardvark.Dom
open Aardvark.Dom.Extensions
open Aardvark.Dom.Extensions.NodeBuilderHelpers

[<AutoOpen>]
module DomExtensions =
    type Attribute with
        member x.ToHtml() =
            let value = 
                match x.Value with
                | AttributeValue.Bool v -> Some (if v then "true" else "false")
                | AttributeValue.String v -> Some v
                | AttributeValue.Int v -> Some (string v)
                | AttributeValue.Set v -> v |> String.concat " " |> Some
                | AttributeValue.Event _ | AttributeValue.Execute _ -> None
                
                
            match value with
            | Some v -> Some $"{x.Name}=\"{value}\""
            | None -> None
    
    type DomNode with
        member x.ToHtml() =
            match x with
            | DomNode.Text content -> content
            | DomNode.VoidElement(tag, att) ->
                AMap.toAVal att.Content |> AVal.map (fun att ->
                    let atts = att |> Seq.choose (fun (k, v) -> Attribute(k,v).ToHtml()) |> String.concat " "
                    $"<{tag} {atts} />"
                )
            | DomNode.Element(tag, att, cs) ->
                let ch = cs |> AList.mapA (fun c -> c.ToHtml())
                (AMap.toAVal att.Content, AList.toAVal ch) ||> AVal.map2 (fun att ch ->
                    let content = String.concat "" ch
                    
                    let atts = att |> Seq.choose (fun (k, v) -> Attribute(k,v).ToHtml()) |> String.concat " "
                    $"<{tag} {atts}>{content}</{tag}>"
                )
            | DomNode.RenderControl _ ->
                AVal.constant "<div>RenderControl</div>"

[<RequireQualifiedAccess>]
type TooltipPlacement =
    | Default
    | Top
    | Bottom
    | Left
    | Right
    

[<CompilerMessage("internal", 3180, IsHidden = true)>]
type TooltipState =
    {
        Placement: TooltipPlacement
        ShowDelay : option<int>
        HideDelay : option<int>
        Animation : option<bool>
    }

[<CompilerMessage("internal", 3180, IsHidden = true)>]
type TooltipBuilder() =
    inherit NodeBuilderHelpers.StateNodeBuilder<TooltipState, AttributeMap>()
    
    member x.Yield (v : TooltipPlacement) =
        x.Yield(fun s -> { s with Placement = v })
    
    override x.Run(action) =
        let s, atts, cs = action { Placement = TooltipPlacement.Default; ShowDelay = Some 300; HideDelay = None; Animation = None }
        let content = 
            cs |> AList.toAVal |> AVal.bind (fun cs ->
                if cs.Count = 0 then
                    AVal.constant ""
                elif cs.Count = 1 then
                    cs.[0].ToHtml()
                else
                    DomNode.Element("div", atts, AList.ofIndexList cs).ToHtml()
            )
        att {
            Attribute("data-bs-toggle", "tooltip")
            Attribute("data-bs-html", "true")
            // match s.Delay with
            // | Some d -> Attribute("data-bs-delay", d)
            // | None -> ()
            match s.Animation with
            | Some d -> Attribute("data-bs-animation", d)
            | None -> ()
            
            match s.Placement with
            | TooltipPlacement.Default -> ()
            | p -> Attribute("data-bs-placement", p.ToString().ToLower())
            
            let delay =
                match s.ShowDelay with
                | Some show ->
                    match s.HideDelay with
                    | Some hide -> Some $"{{ show: {show}, hide: {hide} }}"
                    | None -> Some $"{{ show: {show} }}"
                | None ->
                    match s.HideDelay with
                    | Some hide -> Some $"{{ hide: {hide} }}"
                    | None -> None
                    
            let options =
                match delay with
                | Some d -> $", {{ delay: {d} }}"
                | None -> ""
            AVal.channel content (fun name ->
                [
                    "setTimeout(function() {"
                    $"  if(!__THIS__.tooltip) {{ __THIS__.tooltip = new bootstrap.Tooltip(__THIS__{options}); }}"
                    $"  __THIS__.setAttribute('data-bs-original-title', {name});"
                    $"  try {{ __THIS__.tooltip.getTipElement().getElementsByClassName('tooltip-inner')[0].innerHTML = {name}; }} catch(e) {{}}"
                    "}, 0);"
                ]    
            )
            Attribute("title", AVal.force content)
            Attribute("data-bs-original-title", AVal.force content)
            //content |> AVal.map (fun str -> [Attribute("data-bs-original-title", str)])
        }

[<AutoOpen; CompilerMessage("internal", 3180, IsHidden = true)>]
module TooltipBuilder =
    let tooltip = TooltipBuilder()


type Tooltip =
    static member Placement(p : TooltipPlacement) =
        fun (s : TooltipState) -> { s with Placement = p }
    static member Animation(a : bool) =
        fun (s : TooltipState) -> { s with Animation = Some a }
    static member Delay(a : int) =
        fun (s : TooltipState) -> { s with ShowDelay = Some a; HideDelay = Some a }
    static member ShowDelay(a : int) =
        fun (s : TooltipState) -> { s with ShowDelay = Some a }
    static member HideDelay(a : int) =
        fun (s : TooltipState) -> { s with HideDelay = Some a }