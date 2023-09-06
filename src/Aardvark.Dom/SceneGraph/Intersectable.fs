namespace Aardvark.Dom

open Aardvark.Base
open Aardvark.Base.Geometry
open Aardvark.Geometry
open FSharp.Data.Adaptive
open FShade
open Aardvark.Rendering
open Microsoft.FSharp.NativeInterop

#nowarn "9"

type IIntersectable =
    abstract BoundingBox : Box3d
    abstract Intersects : ray : Ray3d * tmin : float * tmax : float * t : byref<float> * pt : byref<V3d> * n : byref<V3d> -> bool

module Intersectable =

    let box (b : Box3d) =
        { new IIntersectable with
            member x.BoundingBox = b
            member x.Intersects(ray, tmin, tmax, t, hit, n) =
                // o + t*d = b.Min
                let (bmin, bmax) = b.GetMinMaxInDirection(ray.Direction)
                
                let bmin = 
                    if b.Contains(ray.GetPointOnRay tmin) then bmax
                    else bmin
                
                let mutable tt = (bmin - ray.Origin) / ray.Direction
                let inline checkYZ (t : float) =
                    let p = ray.Origin.YZ + t * ray.Direction.YZ
                    b.Min.YZ.AllSmallerOrEqual p && b.Max.YZ.AllGreaterOrEqual p
                let inline checkXY (t : float) =
                    let p = ray.Origin.XY + t * ray.Direction.XY
                    b.Min.XY.AllSmallerOrEqual p && b.Max.XY.AllGreaterOrEqual p
                let inline checkXZ (t : float) =
                    let p = ray.Origin.XZ + t * ray.Direction.XZ
                    b.Min.XZ.AllSmallerOrEqual p && b.Max.XZ.AllGreaterOrEqual p
                    
                if System.Double.IsNaN tt.X || tt.X < tmin || tt.X > tmax || not (checkYZ tt.X) then tt.X <- System.Double.PositiveInfinity
                if System.Double.IsNaN tt.Y || tt.Y < tmin || tt.Y > tmax || not (checkXZ tt.Y) then tt.Y <- System.Double.PositiveInfinity
                if System.Double.IsNaN tt.Z || tt.Z < tmin || tt.Z > tmax || not (checkXY tt.Z)  then tt.Z <- System.Double.PositiveInfinity
                
                let mutable tMin = System.Double.PositiveInfinity
                let mutable nMin = V3d.Zero
                if tt.X <= tt.Y && tt.X <= tt.Z then
                    tMin <- tt.X
                    nMin <- if ray.Direction.X > 0.0 then -V3d.IOO else V3d.IOO
                elif tt.Z <= tt.X && tt.Z <= tt.Y then
                    tMin <- tt.Z
                    nMin <- if ray.Direction.Z > 0.0 then -V3d.OOI else V3d.OOI
                else
                    tMin <- tt.Y
                    nMin <- if ray.Direction.Y > 0.0 then -V3d.OIO else V3d.OIO
                    
                if System.Double.IsFinite tMin && tMin >= tmin && tMin <= tmax then
                    n <- nMin
                    t <- tMin
                    hit <- ray.GetPointOnRay t
                    true
                else
                    false
        }

    let sphere (sphere : Sphere3d) =
        { new IIntersectable with
            member x.BoundingBox = sphere.BoundingBox3d
            member x.Intersects(ray, tmin, tmax, t, hit, n) =
                // |o + t * d| = r
                // |o + t * d|² = r²
                // <o+t*d|o+t*d> = r²
                // <o|o> - r² + 2*t*<d|o> + t²*<d|d> = 0


                let o = ray.Origin - sphere.Center
                let a = Vec.lengthSquared ray.Direction
                let b = 2.0 * Vec.dot ray.Direction o
                let c = Vec.lengthSquared o - sqr sphere.Radius

                let mutable struct(t0, t1) = Polynomial.RealRootsOf(a, b, c)
                if System.Double.IsNaN t0 || t0 < tmin || t0 > tmax then t0 <- System.Double.PositiveInfinity
                if System.Double.IsNaN t1 || t1 < tmin || t1 > tmax then t1 <- System.Double.PositiveInfinity
                let r = min t0 t1

                if System.Double.IsFinite r && r >= tmin && r <= tmax then
                    let n0 = Vec.normalize (ray.GetPointOnRay r - sphere.Center)
                    t <- r
                    n <- n0
                    hit <- ray.GetPointOnRay t
                    true
                else
                    false
        }

    let cylinder (cylinder : Cylinder3d) =
        let d = cylinder.P1 - cylinder.P0
        let h = Vec.length d
        let toCylinder = 
            Trafo3d.Scale(cylinder.Radius, cylinder.Radius, h) *
            Trafo3d.RotateInto(V3d.OOI, d / h) *
            Trafo3d.Translation(cylinder.P0)

        { new IIntersectable with
            member x.BoundingBox =
                cylinder.BoundingBox3d
            member x.Intersects(ray, tmin, tmax, t, hit, n) =
                let ray = ray.Transformed toCylinder.Backward

                let o = ray.Origin
                let d = ray.Direction

                // <o.XY+t*d.XY|o.XY+t*d.XY> = 1
                // (<o.XY|o.XY> - 1) + 2*t*<o.XY|d.XY> t^2*<d.XY|d.XY> = 0
                let mutable struct(t0, t1) = Polynomial.RealRootsOf(d.XY.LengthSquared, 2.0*Vec.dot o.XY d.XY, o.XY.LengthSquared - 1.0)
                let h0 = o.Z + t0 * d.Z
                let h1 = o.Z + t1 * d.Z

                let mutable t2 = -o.Z / d.Z
                let mutable t3 = (1.0 - o.Z) / d.Z
                let r2 = Vec.lengthSquared (o.XY + t2*d.XY)
                let r3 = Vec.lengthSquared (o.XY + t3*d.XY)

                if t0 > tmax || t0 < tmin || h0 < 0.0 || h0 > 1.0 then t0 <- System.Double.PositiveInfinity
                if t1 > tmax || t1 < tmin || h1 < 0.0 || h1 > 1.0 then t1 <- System.Double.PositiveInfinity
                if t2 > tmax || t2 < tmin || r2 > 1.0 then t2 <- System.Double.PositiveInfinity
                if t3 > tmax || t3 < tmin || r3 > 1.0 then t3 <- System.Double.PositiveInfinity

                let nf = if t2 < t3 then V3d.OON else V3d.OOI
                let tf = min t2 t3

                let ts = min t0 t1
                let ns = V3d(Vec.normalize (o.XY + ts * d.XY), 0.0)

                let nr = if tf < ts then nf else ns
                let tr = if tf < ts then tf else ts

                if System.Double.IsFinite tr then
                    t <- tr
                    n <- Vec.normalize (toCylinder.Backward.TransposedTransformDir nr)
                    hit <- ray.GetPointOnRay t
                    true
                else
                    false
        }
    
    let cone (cone : Cone3d) =
        let h = cone.Direction.Length
        let r0 = cone.GetRadius h
        let toCone =
            Trafo3d.Scale(r0, r0, h) *
            Trafo3d.RotateInto(V3d.OOI, cone.Direction / h) *
            Trafo3d.Translation(cone.Origin)

        let bounds =
            let mutable b = cone.GetCircle(h).BoundingBox3d
            b.ExtendBy cone.Origin
            b


        { new IIntersectable with
            member x.BoundingBox =
                bounds
            member x.Intersects(ray, tmin, tmax, t, hit, n) =
                let r = ray.Transformed toCone.Backward
                let d = r.Direction
                let o = r.Origin
                // f := 1

                // rc = (o.Z + t * d.Z) * f
                // rr = sqrt <o.XY + t*d.XY|o.XY + t*d.XY>
                
                // rc² = f²*o.Z² + f²*2.0*o.Z*d.Z*t + f²*d.Z²*t²
                // rr² = <o.XY|o.XY> + 2*<d.XY|o.XY>*t + <d.XY|d.XY>*t²
                
                
                // f²*o.Z²
                // <o.XY|o.XY>
                // rc² - rr² = t² * (f²*d.Z² - <d.XY|d.XY>) + t * (f²*2.0*o.Z*d.Z - 2*<d.XY|o.XY>) + (f²*o.Z² - <o.XY|o.XY>)

                let mutable struct(t0, t1) = 
                    Polynomial.RealRootsOf(
                        sqr d.Z - d.XY.LengthSquared,
                        2.0*(o.Z*d.Z - Vec.dot d.XY o.XY),
                        sqr o.Z - o.XY.LengthSquared
                    )

                let mutable t2 = (1.0 - o.Z) / d.Z

                let z0 = o.Z + t0 * d.Z
                let z1 = o.Z + t1 * d.Z
                let r2 = Vec.lengthSquared (o.XY + t2*d.XY)

                if System.Double.IsNaN t0 || t0 < tmin || t0 > tmax || z0 < 0.0 || z0 > 1.0 then t0 <- System.Double.PositiveInfinity
                if System.Double.IsNaN t1 || t1 < tmin || t1 > tmax || z1 < 0.0 || z1 > 1.0 then t1 <- System.Double.PositiveInfinity
                if System.Double.IsNaN t2 || t2 < tmin || t2 > tmax || r2 > 1.0 then t2 <- System.Double.PositiveInfinity

                let ts = min t0 t1
                
                let tr = min ts t2
                let nr = 
                    if ts < t2 then 
                        V3d(Vec.normalize (o.XY + ts * d.XY), -1.0)
                    else 
                        V3d.OOI
                    
                if System.Double.IsFinite tr then
                    let pt = toCone.Forward.TransformPos (r.GetPointOnRay tr)
                    n <- toCone.Backward.TransposedTransformDir nr |> Vec.normalize
                    t <- tr
                    hit <- ray.GetPointOnRay t
                    true
                else
                    false
        }

    let tetrahedron (p0 : V3d) (p1 : V3d) (p2 : V3d) (p3 : V3d) =
        let bb = Box3d(p0, p1, p2, p3)

        let tri0 = Triangle3d(p0, p2, p1)
        let tri1 = Triangle3d(p0, p1, p3)
        let tri2 = Triangle3d(p0, p3, p2)
        let tri3 = Triangle3d(p1, p2, p3)
        
        let n0 = tri0.Normal |> Vec.normalize
        let n1 = tri1.Normal |> Vec.normalize
        let n2 = tri2.Normal |> Vec.normalize
        let n3 = tri3.Normal |> Vec.normalize

        { new IIntersectable with
            member x.BoundingBox =
                bb
            member x.Intersects(ray, tmin, tmax, t, hit, n) =
                let mutable t0 = System.Double.PositiveInfinity
                let mutable t1 = System.Double.PositiveInfinity
                let mutable t2 = System.Double.PositiveInfinity
                let mutable t3 = System.Double.PositiveInfinity
                if not (tri0.Intersects(ray, &t0)) || t0 < tmin || t0 > tmax then t0 <- System.Double.PositiveInfinity
                if not (tri1.Intersects(ray, &t1)) || t1 < tmin || t1 > tmax then t1 <- System.Double.PositiveInfinity
                if not (tri2.Intersects(ray, &t2)) || t2 < tmin || t2 > tmax then t2 <- System.Double.PositiveInfinity
                if not (tri3.Intersects(ray, &t3)) || t3 < tmin || t3 > tmax then t3 <- System.Double.PositiveInfinity

                let mutable tr = t0
                let mutable nr = n0

                if t1 < tr then
                    tr <- t1
                    nr <- n1
                    
                if t2 < tr then
                    tr <- t2
                    nr <- n2
                    
                if t3 < tr then
                    tr <- t3
                    nr <- n3

                if System.Double.IsFinite tr && tr >= tmin && tr <= tmax then
                    t <- tr
                    n <- nr
                    hit <- ray.GetPointOnRay t
                    true
                else
                    false
        }
    
    let octahedron (p0 : V3d) (p1 : V3d) (p2 : V3d) (p3 : V3d) (t : V3d) (b : V3d) =
        let bb = Box3d [|p0; p1; p2; p3; t; b|]

        let tri0 = Triangle3d(p0, p1, t)
        let tri1 = Triangle3d(p1, p2, t)
        let tri2 = Triangle3d(p2, p3, t)
        let tri3 = Triangle3d(p3, p0, t)
        let tri4 = Triangle3d(p0, b, p1)
        let tri5 = Triangle3d(p1, b, p2)
        let tri6 = Triangle3d(p2, b, p3)
        let tri7 = Triangle3d(p3, b, p0)

        let n0 = tri0.Normal |> Vec.normalize
        let n1 = tri1.Normal |> Vec.normalize
        let n2 = tri2.Normal |> Vec.normalize
        let n3 = tri3.Normal |> Vec.normalize
        let n4 = tri4.Normal |> Vec.normalize
        let n5 = tri5.Normal |> Vec.normalize
        let n6 = tri6.Normal |> Vec.normalize
        let n7 = tri7.Normal |> Vec.normalize

        { new IIntersectable with
            member x.BoundingBox =
                bb
            member x.Intersects(ray, tmin, tmax, t, hit, n) =
                let mutable t0 = System.Double.PositiveInfinity
                let mutable t1 = System.Double.PositiveInfinity
                let mutable t2 = System.Double.PositiveInfinity
                let mutable t3 = System.Double.PositiveInfinity
                let mutable t4 = System.Double.PositiveInfinity
                let mutable t5 = System.Double.PositiveInfinity
                let mutable t6 = System.Double.PositiveInfinity
                let mutable t7 = System.Double.PositiveInfinity
                if not (tri0.Intersects(ray, &t0)) || t0 < tmin || t0 > tmax then t0 <- System.Double.PositiveInfinity
                if not (tri1.Intersects(ray, &t1)) || t1 < tmin || t1 > tmax then t1 <- System.Double.PositiveInfinity
                if not (tri2.Intersects(ray, &t2)) || t2 < tmin || t2 > tmax then t2 <- System.Double.PositiveInfinity
                if not (tri3.Intersects(ray, &t3)) || t3 < tmin || t3 > tmax then t3 <- System.Double.PositiveInfinity
                if not (tri4.Intersects(ray, &t4)) || t4 < tmin || t4 > tmax then t4 <- System.Double.PositiveInfinity
                if not (tri5.Intersects(ray, &t5)) || t5 < tmin || t5 > tmax then t5 <- System.Double.PositiveInfinity
                if not (tri6.Intersects(ray, &t6)) || t6 < tmin || t6 > tmax then t6 <- System.Double.PositiveInfinity
                if not (tri7.Intersects(ray, &t7)) || t7 < tmin || t7 > tmax then t7 <- System.Double.PositiveInfinity

                let mutable tr = t0
                let mutable nr = n0

                if t1 < tr then tr <- t1; nr <- n1
                if t2 < tr then tr <- t2; nr <- n2
                if t3 < tr then tr <- t3; nr <- n3
                if t4 < tr then tr <- t4; nr <- n4
                if t5 < tr then tr <- t5; nr <- n5
                if t6 < tr then tr <- t6; nr <- n6
                if t7 < tr then tr <- t7; nr <- n7

                if System.Double.IsFinite tr && tr >= tmin && tr <= tmax then
                    t <- tr
                    n <- nr
                    hit <- ray.GetPointOnRay t
                    true
                else
                    false
        }

    let planeXY (bounds : Box2d) =
        { new IIntersectable with
            member x.BoundingBox =
                Box3d(V3d(bounds.Min, -1E-8), V3d(bounds.Max, 1E-8))
                
            member x.Intersects(ray, tmin, tmax, t, hit, n) =
                // o.Z + t*d.Z = 0
                let rayT = 
                    if Fun.IsTiny ray.Direction.Z then
                        if Fun.IsTiny ray.Origin.Z then 0.0
                        else System.Double.PositiveInfinity
                    else
                        -ray.Origin.Z / ray.Direction.Z 
                        
                if System.Double.IsFinite rayT && rayT >= tmin && rayT <= tmax then
                    let pt = ray.Origin.XY + ray.Direction.XY * rayT
                    if bounds.Contains pt then
                        t <- rayT
                        n <- V3d.OOI
                        hit <- ray.GetPointOnRay t
                        true
                    else
                        false
                else
                    false
        }
