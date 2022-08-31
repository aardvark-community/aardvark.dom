namespace Aardvark.Dom

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering

type PickObject(state : TraversalState, intersectable : IIntersectable, trafo : aval<Trafo3d>) =
    member x.State = state
    member x.Intersectable = intersectable
    member x.Trafo = trafo

module RenderObject =

    type TraversalStateUniformProvider(state : TraversalState) =
        static let camCache = UnaryCache<aval<Trafo3d>, aval<V3d>>(AVal.map (fun v -> v.GetViewPosition()))
        static let invCache = UnaryCache<aval<Trafo3d>, aval<Trafo3d>>(AVal.map (fun v -> v.Inverse))
        static let normalMatrixCache = UnaryCache<aval<Trafo3d>, aval<M33d>>(AVal.map (fun v -> M33d v.Backward.Transposed))
        
        let model = 
            lazy (TraversalState.modelTrafo state)


        member x.TryGetUniform(name : string) =
            match HashMap.tryFind name state.Uniforms with
            | None ->
                match name with
                | "NormalMatrix" -> normalMatrixCache.Invoke model.Value :> IAdaptiveValue |> Some
                | "ModelTrafo" -> model.Value :> IAdaptiveValue |> Some
                | "ModelTrafoInv" -> invCache.Invoke model.Value :> IAdaptiveValue |> Some
                | "ViewTrafo" -> state.View :> IAdaptiveValue |> Some
                | "ProjTrafo" -> state.Proj :> IAdaptiveValue |> Some
                | "ViewTrafoInv" -> invCache.Invoke state.View :> IAdaptiveValue |> Some
                | "ProjTrafoInv" -> invCache.Invoke state.Proj :> IAdaptiveValue |> Some
                | "ViewProjTrafo" -> (state.View,state.Proj) ||> AVal.map2 (*) :> IAdaptiveValue |> Some
                | "CameraLocation" -> camCache.Invoke state.View :> IAdaptiveValue |> Some
                | "LightLocation" -> camCache.Invoke state.View :> IAdaptiveValue |> Some
                | _ ->  
                    if name.StartsWith "Has" then
                        let result = 
                            let attName = name.Substring 3
                            HashMap.containsKey attName state.VertexAttributes ||
                            HashMap.containsKey attName state.InstanceAttributes
                        AVal.constant result :> IAdaptiveValue |> Some
                    else
                        None
            | value ->
                value

        interface IUniformProvider with
            member x.Dispose() =
                ()
            member x.TryGetUniform(_, name) =
                x.TryGetUniform (string name)
                
    type AdapterUniformProvider(state : TraversalState) =
        member x.TryGetUniform(name : string) =
            HashMap.tryFind name state.Uniforms

        interface IUniformProvider with
            member x.Dispose() =
                ()
            member x.TryGetUniform(_, name) =
                x.TryGetUniform (string name)

    let traversalStates = System.Runtime.CompilerServices.ConditionalWeakTable<IRenderObject, TraversalState>()

    let ofTraversalState (state : TraversalState) =
        

        let provider (atts : HashMap<string, BufferView>) =
            { new IAttributeProvider with
                member x.Dispose() =
                    ()
                member x.TryGetAttribute(name) =
                    HashMap.tryFind (string name) atts
                member x.All =
                    Seq.empty
            }

        let o = RenderObject.Create()
        lock traversalStates (fun () -> traversalStates.Add(o, state) |> ignore)
        o.BlendState <- state.Blend
        o.DepthState <- state.Depth
        o.DrawCalls <- DrawCalls.Direct (AVal.constant [])
        o.Indices <- state.Index
        o.InstanceAttributes <- provider state.InstanceAttributes
        o.VertexAttributes <- provider state.VertexAttributes
        o.IsActive <- state.Active
        o.Mode <- state.Mode
        o.RasterizerState <- state.Rasterizer
        o.RenderPass <- state.Pass
        o.StencilState <- state.Stencil
        o.Surface <- Surface.FShadeSimple state.Shader
        o.Uniforms <- new TraversalStateUniformProvider(state)
        o


type ISceneNode =
    abstract GetObjects : TraversalState -> aset<IRenderObject> * aset<PickObject>
    abstract GetRenderObjects : TraversalState -> aset<IRenderObject>
    
type DirectDrawNode(call : aval<list<DrawCallInfo>>) =
    member x.Call = call

    interface ISceneNode with
        member x.GetObjects(state : TraversalState) =
            let o = RenderObject.ofTraversalState state
            o.DrawCalls <- DrawCalls.Direct call
            ASet.single (o :> IRenderObject), ASet.empty

        member x.GetRenderObjects(state : TraversalState) =
            let o = RenderObject.ofTraversalState state
            o.DrawCalls <- DrawCalls.Direct call
            ASet.single (o :> IRenderObject)

    new(call : aval<DrawCallInfo>) = DirectDrawNode(AVal.mapNonAdaptive List.singleton call)
    new(calls : list<DrawCallInfo>) = DirectDrawNode(AVal.constant calls)
    new(call : DrawCallInfo) = DirectDrawNode(AVal.constant [call])

type IndirectDrawNode(buffer : aval<IndirectBuffer>) =
    member x.Buffer = buffer

    interface ISceneNode with
        member x.GetObjects(state : TraversalState) =
            let o = RenderObject.ofTraversalState state
            o.DrawCalls <- DrawCalls.Indirect buffer
            ASet.single (o :> IRenderObject), ASet.empty
        member x.GetRenderObjects(state : TraversalState) =
            let o = RenderObject.ofTraversalState state
            o.DrawCalls <- DrawCalls.Indirect buffer
            ASet.single (o :> IRenderObject)

    new(buffer : IndirectBuffer) = IndirectDrawNode(AVal.constant buffer)

type Applicator(attributes : list<SceneAttribute>, children : aset<ISceneNode>) =
    
    member x.Attributes = attributes
    member x.Children = children

    member x.GetChildState (state : TraversalState) =
        let state = TraversalState.push state
        let mutable state = state
        let mutable trafos = System.Collections.Generic.List()
        for a in attributes do
            match a with
            | SceneAttribute.Model m -> trafos.AddRange m
            | a -> state <- SceneAttribute.apply a state
        if trafos.Count > 0 then
            state <- { state with Model = CSharpList.toList trafos @ state.Model }
        state
    
    interface ISceneNode with
        member x.GetObjects(state : TraversalState) =
            
            let self =
                (([], None), attributes) ||> List.fold (fun (m, i) a ->
                    match i with
                    | Some t -> m, Some t
                    | None -> 
                        match a with 
                        | SceneAttribute.Intersectable i -> m, Some (m, i)
                        | SceneAttribute.Model ms -> (m @ ms), None
                        | _ -> (m, None)
                )
                |> snd
                
            let childState = x.GetChildState state
            match self with
            | Some (model, res) ->
                let trafo = { state with Model = model @ state.Model } |> TraversalState.modelTrafo
                let pick = 
                    res |> ASet.bind (fun i ->
                        ASet.single (PickObject(state, i, trafo))
                    )
                let render =
                    children |> ASet.collect (fun c -> c.GetRenderObjects childState)
                render, pick
            | None ->
                let noEvents = attributes |> List.exists (function SceneAttribute.NoEvents -> true | _ -> false)
                if noEvents then
                    let render =
                        children |> ASet.collect (fun c -> c.GetRenderObjects childState)
                    render, ASet.empty
                else
                    let things = children |> ASet.map (fun c -> c.GetObjects childState)
                    let render = things |> ASet.collect fst
                    let pick = things |> ASet.collect snd
                    render, pick
                
        member x.GetRenderObjects(state : TraversalState) =
            let childState = x.GetChildState state
            children |> ASet.collect (fun c -> c.GetRenderObjects childState)
      
module SgAdapter =
    open Aardvark.Base.Ag
    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Semantics
           
    type WrapperNode(sg : ISg, state : TraversalState) =
        interface ISg
        member x.Node = sg
        member x.State = state

    type Node(sg : ISg) =
        member x.GetRenderObjects(state : TraversalState) =
            WrapperNode(sg, state).RenderObjects(Ag.Scope.Root)
     
        interface ISceneNode with
            member x.GetRenderObjects(state) = x.GetRenderObjects state
            member x.GetObjects(state) = x.GetRenderObjects state, ASet.empty

    [<Rule>]
    type WrapperSemantic() =
    
        static let getViewPosition (viewTrafo : Trafo3d) = 
            viewTrafo.GetViewPosition()

        static let bufferViewCount (view : BufferView) =
            if view.IsSingleValue then
                AVal.constant 0
            else
                let elementSize = System.Runtime.InteropServices.Marshal.SizeOf view.ElementType
                view.Buffer |> AVal.map (fun b ->
                    match b with
                        | :? INativeBuffer as b -> (b.SizeInBytes - view.Offset) / elementSize
                        | _ -> failwithf "[Sg] could not determine buffer-size: %A" b
                )
                
        member x.Runtime(sg : WrapperNode, _scope : Ag.Scope) =
            sg.Node?Runtime <- sg.State.Runtime
            
        member x.Uniforms(sg : WrapperNode, _scope : Ag.Scope) =
            sg.Node?Uniforms <- [new RenderObject.AdapterUniformProvider(sg.State) :> IUniformProvider]
            
        member x.InstanceAttributes(sg : WrapperNode, _scope : Ag.Scope) =
            let att = sg.State.InstanceAttributes |> Seq.map (fun (n, v) -> Symbol.Create n, v) |> Map.ofSeq
            sg.Node?InstanceAttributes <- att
            
        member x.VertexAttributes(sg : WrapperNode, _scope : Ag.Scope) =
            let att = sg.State.VertexAttributes |> Seq.map (fun (n, v) -> Symbol.Create n, v) |> Map.ofSeq
            sg.Node?VertexAttributes <- att
            
        member x.VertexIndexBuffer(sg : WrapperNode, _scope : Ag.Scope) =
            sg.Node?VertexIndexBuffer <- sg.State.Index
            
        member x.FaceVertexCount(sg : WrapperNode, _scope : Ag.Scope) =
            match sg.State.Index with
            | Some b ->
                sg.Node?FaceVertexCount <- bufferViewCount b
            | _ -> 
                match HashMap.tryFind (string DefaultSemantic.Positions) sg.State.VertexAttributes with
                | Some pos ->
                    sg.Node?FaceVertexCount <- bufferViewCount pos
                | _ -> 
                    sg.Node?FaceVertexCount <- AVal.constant 0

        member x.Surface(sg : WrapperNode, _scope : Ag.Scope) =
            sg.Node?Surface <- Surface.FShadeSimple sg.State.Shader

        member x.CameraLocation(e : WrapperNode, _scope : Ag.Scope) =
            e.Node?CameraLocation <- AVal.map getViewPosition e.State.View
            
        member x.RenderPass(e : WrapperNode, _scope : Ag.Scope) =
            e.Node?RenderPass <- e.State.Pass
            
        member x.IsActive(e : WrapperNode, _scope : Ag.Scope) =
            e.Node?IsActive <- e.State.Active
            
        member x.BlendMode(e : WrapperNode, _scope : Ag.Scope) = e.Node?BlendMode <- e.State.Blend.Mode
        member x.BlendConstant(e : WrapperNode, _scope : Ag.Scope) = e.Node?BlendConstant <- e.State.Blend.ConstantColor
        member x.ColorWriteMask(e : WrapperNode, _scope : Ag.Scope) = e.Node?ColorWriteMask <- e.State.Blend.ColorWriteMask
        member x.AttachmentBlendMode(e : WrapperNode, _scope : Ag.Scope) = e.Node?AttachmentBlendMode <- e.State.Blend.AttachmentMode
        member x.AttachmentColorWriteMask(e : WrapperNode, _scope : Ag.Scope) = e.Node?AttachmentColorWriteMask <- e.State.Blend.AttachmentWriteMask
        
        member x.DepthTest(e : WrapperNode, _scope : Ag.Scope) = e.Node?DepthTest <- e.State.Depth.Test
        member x.DepthBias(e : WrapperNode, _scope : Ag.Scope) = e.Node?DepthBias <- e.State.Depth.Bias
        member x.DepthWriteMask(e : WrapperNode, _scope : Ag.Scope) = e.Node?DepthWriteMask <- e.State.Depth.WriteMask
        member x.DepthClamp(e : WrapperNode, _scope : Ag.Scope) = e.Node?DepthClamp <- e.State.Depth.Clamp

        
        member x.StencilModeFront(e : WrapperNode, _scope : Ag.Scope) = e.Node?StencilModeFront <- e.State.Stencil.ModeFront
        member x.StencilWriteMaskFront(e : WrapperNode, _scope : Ag.Scope) = e.Node?StencilWriteMaskFront <- e.State.Stencil.WriteMaskFront
        member x.StencilModeBack(e : WrapperNode, _scope : Ag.Scope) = e.Node?StencilModeBack <- e.State.Stencil.ModeBack
        member x.StencilWriteMaskBack(e : WrapperNode, _scope : Ag.Scope) = e.Node?StencilWriteMaskBack <- e.State.Stencil.WriteMaskBack

        member x.CullMode(e : WrapperNode, _scope : Ag.Scope) = e.Node?CullMode <- e.State.Rasterizer.CullMode
        member x.FrontFace(e : WrapperNode, _scope : Ag.Scope) = e.Node?FrontFace <- e.State.Rasterizer.FrontFace
        member x.FillMode(e : WrapperNode, _scope : Ag.Scope) = e.Node?FillMode <- e.State.Rasterizer.FillMode
        member x.Multisample(e : WrapperNode, _scope : Ag.Scope) = e.Node?Multisample <- e.State.Rasterizer.Multisample
        member x.ConservativeRaster(e : WrapperNode, _scope : Ag.Scope) = e.Node?ConservativeRaster <- e.State.Rasterizer.ConservativeRaster

        
        member x.ModelTrafoStack(sg : WrapperNode, _scope : Ag.Scope) =
            sg.Node?ModelTrafoStack <- sg.State.Model

        member x.ViewTrafo(sg : WrapperNode, _scope : Ag.Scope) =
            sg.Node?ViewTrafo <- sg.State.View

        member x.ProjTrafo(sg : WrapperNode, _scope : Ag.Scope) =
            sg.Node?ProjTrafo <- sg.State.Proj

        member x.RenderObjects(sg : WrapperNode, scope : Ag.Scope) : aset<IRenderObject> =
            sg.Node.RenderObjects(scope) |> ASet.map (fun o ->
                RenderObject.traversalStates.Add(o, sg.State) |> ignore
                o
            )
            


type ShaderAttributeBuilder() =
    member x.Zero() = []
    member x.Yield (shader : 'a -> Microsoft.FSharp.Quotations.Expr<'b>) =
        [FShade.Effect.ofFunction shader]
    member x.Delay (action : unit -> list<FShade.Effect>) = action
    member x.Combine(l : list<FShade.Effect>, r : unit -> list<FShade.Effect>) =
        l @ r()
    member x.Run(l : unit -> list<FShade.Effect>) =
        FShade.Effect.compose (l()) |> SceneAttribute.Shader

type UniformsBuilder() =
    member x.Zero() = HashMap.empty
    member x.Yield((name : string, value : aval<'a>)) =
        HashMap.single name (value :> IAdaptiveValue)
    
    member x.Yield<'a when 'a : unmanaged>((name : string, value : 'a)) =
        HashMap.single name (AVal.constant value :> IAdaptiveValue)

    member x.Delay (action : unit -> HashMap<string, IAdaptiveValue>) = action

    member x.Combine(l : HashMap<string, IAdaptiveValue>, r : unit -> HashMap<string, IAdaptiveValue>) =
        HashMap.union l (r())

    member x.Run(l : unit -> HashMap<string, IAdaptiveValue>) =
        SceneAttribute.Uniforms (l())

type NodeBuilderState() =
    let mutable events = AMap.empty
    let mutable attributes = System.Collections.Generic.List()
    let mutable stack = []

    let rec choose2 (mapping : 'a -> Choice<'b, 'c>) (l : list<'a>) =
        match l with
        | [] -> [], []
        | a :: rest ->
            let l, r = choose2 mapping rest
            match mapping a with
            | Choice1Of2 b -> b :: l, r
            | Choice2Of2 c -> l, c :: r

    member x.Append(node : aset<ISceneNode>) =
        if attributes.Count > 0 then 
            stack <- (CSharpList.toList attributes, [node]) :: stack
            attributes.Clear()
        else
            match stack with
            | (hatt, hnodes) :: rest ->
                stack <- (hatt, node :: hnodes) :: rest
            | [] ->
                stack <- ([], [node]) :: stack

    member x.Append(att : list<SceneAttribute>) =
        let evts, things = att |> choose2 (function SceneAttribute.On m -> Choice1Of2 m | a -> Choice2Of2 a)
        attributes.AddRange things
        events <- (events, evts) ||> List.fold (AMap.unionWith (fun _ -> SceneEventHandler.merge))

    member x.Build() =

        let inline union (l : list<aset<'a>>) =
            let c, d = l |> List.partition (fun s -> s.IsConstant)
            let c = c |> List.map ASet.force |> HashSet.unionMany
            let d = ASet.ofList d |> ASet.unionMany
            ASet.union (ASet.ofHashSet c) d

        let rec run (acc : option<Applicator>) (stack : list<list<SceneAttribute> * list<aset<ISceneNode>>>) =
            match stack with
            | [(att, children)] ->
                match acc with
                | Some acc -> 
                    Applicator(att, union (ASet.single acc :: children))
                | None ->
                    Applicator(att, union children)
                    
            | [] ->
                match acc with
                | Some acc -> acc
                | None -> Applicator([], ASet.empty)
            | (att, children) :: t ->
                let self = 
                    match acc with
                    | Some acc -> Applicator(att, union (ASet.single acc :: children))
                    | None -> Applicator(att, union children)
                run (Some self) t

        try 
            let result = run None stack
            let noEvent = events.IsConstant && HashMap.isEmpty (AMap.force events)
            if noEvent then result :> ISceneNode
            else Applicator(SceneAttribute.On(events) :: result.Attributes, result.Children) :> ISceneNode
        finally
            stack <- []
            attributes.Clear()

type Builder<'a> = NodeBuilderState -> 'a

type SceneNodeBuilder() =

    static let wrap (sg : Aardvark.SceneGraph.ISg) =
        SgAdapter.Node(sg) :> ISceneNode

    member x.Yield(att : SceneAttribute) : Builder<unit> =
        fun (s : NodeBuilderState) -> s.Append [att]

    member x.Yield(node : aset<ISceneNode>) : Builder<unit> =
        fun (s : NodeBuilderState) -> s.Append node
        
    member x.Zero() : Builder<unit> =
        fun (s : NodeBuilderState) -> ()
        
    member x.Delay(action : unit -> Builder<'a>) : Builder<'a> =
        action()
        
    member x.Combine(l : Builder<unit>, r : Builder<'a>) : Builder<'a> =
        fun s -> l s; r s

    member x.Run(action : Builder<unit>) : ISceneNode =
        let s = NodeBuilderState()
        action s
        s.Build()
     
        
    member x.Yield(node : ISceneNode) : Builder<unit> =
        x.Yield(ASet.single node)
        
    member x.Bind((info : 'a, node : ISceneNode), action : 'a -> Builder<'b>) : Builder<'b> =
        x.Combine(x.Yield(node), action info)

    member x.Yield(node : Aardvark.SceneGraph.ISg) : Builder<unit> =
        x.Yield(ASet.single (wrap node))

    member x.Yield(node : aval<#seq<ISceneNode>>) : Builder<unit> =
        x.Yield(ASet.ofAVal node)
        
    member x.Yield(node : aval<seq<Aardvark.SceneGraph.ISg>>) : Builder<unit> =
        x.Yield(ASet.ofAVal node |> ASet.map wrap)

    member x.Yield(node : aval<ISceneNode>) : Builder<unit> =
        x.Yield(node |> ASet.bind ASet.single)
        
    member x.Yield(node : aval<Aardvark.SceneGraph.ISg>) : Builder<unit>=
        x.Yield(node |> ASet.bind (wrap >> ASet.single))
        
    member x.Yield(node : aval<option<ISceneNode>>) : Builder<unit> =
        x.Yield(node |> ASet.bind (function Some v -> ASet.single v | None -> ASet.empty))
        
    member x.Yield(node : aval<option<Aardvark.SceneGraph.ISg>>) : Builder<unit> =
        x.Yield(node |> ASet.bind (function Some v -> ASet.single (wrap v) | None -> ASet.empty))
        