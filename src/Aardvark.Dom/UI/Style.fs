namespace Aardvark.Dom

open Aardvark.Base
open FSharp.Data.Adaptive

[<Struct>]
type StyleProperty(name : string, value : string) =
    member x.Name = name
    member x.Value = value


[<AbstractClass; Sealed; AutoOpen>]
type Css private() =
    static member Color(value : string) = StyleProperty("color", value)
    static member Cursor(value : string) = StyleProperty("cursor", value)
    static member FontFamily(value : string) = StyleProperty("font-family", value)
    static member FontSize(value : string) = StyleProperty("font-size", value)
    static member FontStyle(value : string) = StyleProperty("font-style", value)
    static member FontWeight(value : string) = StyleProperty("font-weight", value)
    static member ListStyleType(value : string) = StyleProperty("list-style-type", value)
    static member TextDecoration(value : string) = StyleProperty("text-decoration", value)
    static member TextAlign(value : string) = StyleProperty("text-align", value)
    static member VerticalAlign(value : string) = StyleProperty("vertical-align", value)
    static member Display(value : string) = StyleProperty("display", value)
    static member Position(value : string) = StyleProperty("position", value)
    static member Float(value : string) = StyleProperty("float", value)
    static member Clear(value : string) = StyleProperty("clear", value)
    static member Margin(value : string) = StyleProperty("margin", value)
    static member MarginTop(value : string) = StyleProperty("margin-top", value)
    static member MarginRight(value : string) = StyleProperty("margin-right", value)
    static member MarginBottom(value : string) = StyleProperty("margin-bottom", value)
    static member MarginLeft(value : string) = StyleProperty("margin-left", value)
    static member Padding(value : string) = StyleProperty("padding", value)
    static member PaddingTop(value : string) = StyleProperty("padding-top", value)
    static member PaddingRight(value : string) = StyleProperty("padding-right", value)
    static member PaddingBottom(value : string) = StyleProperty("padding-bottom", value)
    static member PaddingLeft(value : string) = StyleProperty("padding-left", value)
    static member Border(value : string) = StyleProperty("border", value)
    static member BorderTop(value : string) = StyleProperty("border-top", value)
    static member BorderRight(value : string) = StyleProperty("border-right", value)
    static member BorderBottom(value : string) = StyleProperty("border-bottom", value)
    static member BorderLeft(value : string) = StyleProperty("border-left", value)
    static member BorderWidth(value : string) = StyleProperty("border-width", value)
    static member BorderTopWidth(value : string) = StyleProperty("border-top-width", value)
    static member BorderRightWidth(value : string) = StyleProperty("border-right-width", value)
    static member BorderBottomWidth(value : string) = StyleProperty("border-bottom-width", value)
    static member BorderLeftWidth(value : string) = StyleProperty("border-left-width", value)
    static member BorderStyle(value : string) = StyleProperty("border-style", value)
    static member BorderTopStyle(value : string) = StyleProperty("border-top-style", value)
    static member BorderRightStyle(value : string) = StyleProperty("border-right-style", value)
    static member BorderBottomStyle(value : string) = StyleProperty("border-bottom-style", value)
    static member BorderLeftStyle(value : string) = StyleProperty("border-left-style", value)
    static member BorderColor(value : string) = StyleProperty("border-color", value)
    static member BorderTopColor(value : string) = StyleProperty("border-top-color", value)
    static member BorderRightColor(value : string) = StyleProperty("border-right-color", value)
    static member BorderBottomColor(value : string) = StyleProperty("border-bottom-color", value)
    static member BorderLeftColor(value : string) = StyleProperty("border-left-color", value)
    static member BorderRadius(value : string) = StyleProperty("border-radius", value)
    static member BorderTopLeftRadius(value : string) = StyleProperty("border-top-left-radius", value)
    static member BorderTopRightRadius(value : string) = StyleProperty("border-top-right-radius", value)
    static member BorderBottomLeftRadius(value : string) = StyleProperty("border-bottom-left-radius", value)
    static member BorderBottomRightRadius(value : string) = StyleProperty("border-bottom-right-radius", value)
    static member Width(value : string) = StyleProperty("width", value)
    static member Height(value : string) = StyleProperty("height", value)
    static member MinWidth(value : string) = StyleProperty("min-width", value)
    static member MinHeight(value : string) = StyleProperty("min-height", value)
    static member MaxWidth(value : string) = StyleProperty("max-width", value)
    static member MaxHeight(value : string) = StyleProperty("max-height", value)
    static member Overflow(value : string) = StyleProperty("overflow", value)
    static member OverflowX(value : string) = StyleProperty("overflow-x", value)
    static member OverflowY(value : string) = StyleProperty("overflow-y", value)
    static member Visibility(value : string) = StyleProperty("visibility", value)
    static member Top(value : string) = StyleProperty("top", value)
    static member Right(value : string) = StyleProperty("right", value)
    static member Bottom(value : string) = StyleProperty("bottom", value)
    static member Left(value : string) = StyleProperty("left", value)
    static member Clip(value : string) = StyleProperty("clip", value)
    static member ClipTop(value : string) = StyleProperty("clip-top", value)
    static member ClipRight(value : string) = StyleProperty("clip-right", value)
    static member ClipBottom(value : string) = StyleProperty("clip-bottom", value)
    static member ClipLeft(value : string) = StyleProperty("clip-left", value)
    static member Opacity(value : string) = StyleProperty("opacity", value)
    static member Filter(value : string) = StyleProperty("filter", value)
    static member ZIndex(value : int) = StyleProperty("z-index", string value)
    static member Background(value : string) = StyleProperty("background", value)
    static member BackgroundColor(value : string) = StyleProperty("background-color", value)
    static member BackgroundImage(value : string) = StyleProperty("background-image", value)
    static member BackgroundPosition(value : string) = StyleProperty("background-position", value)
    static member BackgroundRepeat(value : string) = StyleProperty("background-repeat", value)
    static member BackgroundAttachment(value : string) = StyleProperty("background-attachment", value)
    static member BackgroundSize(value : string) = StyleProperty("background-size", value)
    static member BackgroundOrigin(value : string) = StyleProperty("background-origin", value)
    static member BackgroundClip(value : string) = StyleProperty("background-clip", value)

    static member FontSize(value : int) =
        StyleProperty("font-size", sprintf "%dpx" value)

    static member FontSize(value : float) =
        StyleProperty("font-size", sprintf "%fpx" value)

    static member Color(c : C3b) = 
        StyleProperty("color", sprintf "#%02X%02X%02X" c.R c.G c.B)

    static member Color(c : C4b) = 
        if c.A = 255uy then StyleProperty("color", sprintf "#%02X%02X%02X" c.R c.G c.B)
        else StyleProperty("color", sprintf "rgba(%d, %d, %d, %f)"  c.R c.G c.B (float c.A / 255.0))

    static member Background(c : C3b) = 
        StyleProperty("background", sprintf "#%02X%02X%02X" c.R c.G c.B)
        
    static member Background(c : C4b) = 
        if c.A = 255uy then StyleProperty("background", sprintf "#%02X%02X%02X" c.R c.G c.B)
        else StyleProperty("background", sprintf "rgba(%d, %d, %d, %f)"  c.R c.G c.B (float c.A / 255.0))

    static member Color(c : C4f) =
        let r = (if c.R < 0.0f then 0.0f elif c.R > 1.0f then 1.0f else c.R) * 255.0f |> round |> int
        let g = (if c.G < 0.0f then 0.0f elif c.G > 1.0f then 1.0f else c.G) * 255.0f |> round |> int
        let b = (if c.B < 0.0f then 0.0f elif c.B > 1.0f then 1.0f else c.B) * 255.0f |> round |> int
        if c.A >= 1.0f then StyleProperty("color", sprintf "#%02X%02X%02X" r g b)
        else StyleProperty("color", sprintf "rgba(%d, %d, %d, %.5f)" r g b (max 0.0f c.A))

    static member Color(c : C3f) =
        let r = (if c.R < 0.0f then 0.0f elif c.R > 1.0f then 1.0f else c.R) * 255.0f |> round |> int
        let g = (if c.G < 0.0f then 0.0f elif c.G > 1.0f then 1.0f else c.G) * 255.0f |> round |> int
        let b = (if c.B < 0.0f then 0.0f elif c.B > 1.0f then 1.0f else c.B) * 255.0f |> round |> int
        StyleProperty("background", sprintf "#%02X%02X%02X" r g b)

    static member Color(r : float, g : float, b : float, a : float) = 
        let r = (if r < 0.0 then 0.0 elif r > 1.0 then 1.0 else r) * 255.0 |> round |> int
        let g = (if g < 0.0 then 0.0 elif g > 1.0 then 1.0 else g) * 255.0 |> round |> int
        let b = (if b < 0.0 then 0.0 elif b > 1.0 then 1.0 else b) * 255.0 |> round |> int
        if a >= 1.0 then StyleProperty("background", sprintf "#%02X%02X%02X" r g b)
        else StyleProperty("color", sprintf "rgba(%d, %d, %d, %.5f)" r g b (max 0.0 a))

    static member Color(r : float, g : float, b : float) =
        Css.Color(r, g, b, 1.0)
        
    static member Background(r : float, g : float, b : float, a : float) = 
        let r = (if r < 0.0 then 0.0 elif r > 1.0 then 1.0 else r) * 255.0 |> round |> int
        let g = (if g < 0.0 then 0.0 elif g > 1.0 then 1.0 else g) * 255.0 |> round |> int
        let b = (if b < 0.0 then 0.0 elif b > 1.0 then 1.0 else b) * 255.0 |> round |> int
        if a >= 1.0 then StyleProperty("background", sprintf "#%02X%02X%02X" r g b)
        else StyleProperty("background", sprintf "rgba(%d, %d, %d, %.5f)" r g b (max 0.0 a))

    static member Background(r : float, g : float, b : float) =
        Css.Background(r, g, b, 1.0)
        