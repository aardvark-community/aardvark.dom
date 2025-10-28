namespace Aardvark.Dom

open System
open Aardvark.Base
open Aardvark.Dom
open System.Threading.Tasks


[<AutoOpen>]
module Dom =

    let inline RandomElementId() =
        let g = System.Guid.NewGuid()
        let b = g.ToByteArray() |> System.Convert.ToBase64String
        let b = b.Replace("/", "_").Replace("=", "").Replace("+", "Aa")
        $"N{b}"
        
    
    let renderControl = RenderControlBuilder()

    let h1 = NodeBuilder "h1"
    let code = NodeBuilder "code"
    let pre = NodeBuilder "pre"
    let span = NodeBuilder "span"
    let div = NodeBuilder "div"
    let table = NodeBuilder "table"
    let tr = NodeBuilder "tr"
    let td = NodeBuilder "td"
    let ul = NodeBuilder "ul"
    let li = NodeBuilder "li"
    let a = NodeBuilder "a"
    let p = NodeBuilder "p"
    let button = NodeBuilder "button"
    let input = NodeBuilder "input"
    let textarea = NodeBuilder "textarea"
    let img = NodeBuilder "img"
    let br = VoidNodeBuilder "br"
    let hr = NodeBuilder "hr"
    let h2 = NodeBuilder "h2"
    let h3 = NodeBuilder "h3"
    let h4 = NodeBuilder "h4"
    let h5 = NodeBuilder "h5"
    let h6 = NodeBuilder "h6"
    let b = NodeBuilder "b"
    let i = NodeBuilder "i"
    let strong = NodeBuilder "strong"
    let em = NodeBuilder "em"
    let small = NodeBuilder "small"
    let sub = NodeBuilder "sub"
    let sup = NodeBuilder "sup"
    let s = NodeBuilder "s"
    let u = NodeBuilder "u"
    let ol = NodeBuilder "ol"
    let dl = NodeBuilder "dl"
    let dt = NodeBuilder "dt"
    let dd = NodeBuilder "dd"
    let cite = NodeBuilder "cite"
    let blockquote = NodeBuilder "blockquote"
    let q = NodeBuilder "q"
    let abbr = NodeBuilder "abbr"
    let acronym = NodeBuilder "acronym"
    let address = NodeBuilder "address"
    let bdo = NodeBuilder "bdo"
    let big = NodeBuilder "big"
    let blink = NodeBuilder "blink"
    let center = NodeBuilder "center"
    let cnter = NodeBuilder "cnter"
    let strike = NodeBuilder "strike"
    let dir = NodeBuilder "dir"
    let font = NodeBuilder "font"
    let nobr = NodeBuilder "nobr"
    let noembed = NodeBuilder "noembed"
    let noframes = NodeBuilder "noframes"
    let plaintext = NodeBuilder "plaintext"
    let samp = NodeBuilder "samp"
    let select = NodeBuilder "select"
    let tt = NodeBuilder "tt"
    let xmp = NodeBuilder "xmp"
    let marquee = NodeBuilder "marquee"
    let area = NodeBuilder "area"
    let basefont = NodeBuilder "basefont"
    let col = NodeBuilder "col"
    let colgroup = NodeBuilder "colgroup"
    let frame = NodeBuilder "frame"
    let isindex = NodeBuilder "isindex"
    let link = NodeBuilder "link"
    let meta = NodeBuilder "meta"
    let param = NodeBuilder "param"
    let embed = NodeBuilder "embed"
    let base_ = NodeBuilder "base"
    let body = NodeBuilder "body"
    let head = NodeBuilder "head"
    let html = NodeBuilder "html"
    let form = NodeBuilder "form"
    let fieldset = NodeBuilder "fieldset"
    let legend = NodeBuilder "legend"
    let optgroup = NodeBuilder "optgroup"
    let option = NodeBuilder "option"
    let script = NodeBuilder "script"
    let style = NodeBuilder "style"
    let title = NodeBuilder "title"
    let caption = NodeBuilder "caption"
    let colgroup_ = NodeBuilder "colgroup"
    let table_ = NodeBuilder "table"
    let tbody = NodeBuilder "tbody"
    let thead = NodeBuilder "thead"
    let tfoot = NodeBuilder "tfoot"
    let iframe = NodeBuilder "iframe"
    let frame_ = NodeBuilder "frame"
    let frameset = NodeBuilder "frameset"
    let frame_set = NodeBuilder "frame_set"
    let label = NodeBuilder "label"
    let option_ = NodeBuilder "option"
    let optgroup_ = NodeBuilder "optgroup"
    let keygen = NodeBuilder "keygen"
    let output = NodeBuilder "output"
    let progress = NodeBuilder "progress"
    let meter = NodeBuilder "meter"
    let details = NodeBuilder "details"
    let summary = NodeBuilder "summary"
    let datalist = NodeBuilder "datalist"
    let menu = NodeBuilder "menu"
    let menuitem = NodeBuilder "menuitem"
    let multicol = NodeBuilder "multicol"

[<AbstractClass; Sealed; AutoOpen>]
type Dom private() =
    
    /// A Data-Attribute with name `data-{name}`
    static member Data(name : string, value : string) =
        Attribute($"data-{name}", AttributeValue.String value)
    
    /// The accept attribute specifies the types of files that the server accepts (that can be submitted through a file upload).
    /// Note: The accept attribute can only be used with <input type="file">.
    static member inline Accept (typ : string) = Attribute("accept", AttributeValue.String typ)

    /// The accept-charset attribute specifies the character encodings that are to be used for the form submission.
    /// The default value is the reserved string "UNKNOWN" (indicates that the encoding equals the encoding of the document containing the <form> element).
    static member inline AcceptCharset (charset : string) = Attribute("accept-charset", AttributeValue.String charset)

    /// The accesskey attribute specifies a shortcut key to activate/focus an element.
    static member inline AccessKey (key : string) = Attribute("accessKey ", AttributeValue.String key)

    /// The action attribute specifies where to send the form-data when a form is submitted.
    static member inline Action (action : string) = Attribute("action", AttributeValue.String action)

    /// The alt attribute provides alternative information for an image if a user for some reason cannot view it (because of slow connection, an error in the src attribute, or if the user uses a screen reader).
    static member inline Alt (alt  : string) = Attribute("alt", AttributeValue.String alt)
    
    /// When present, it specifies that the script will be executed asynchronously as soon as it is available.
    static member inline Async (value  : bool) = Attribute("async", AttributeValue.Bool value)

    /// The autocomplete attribute specifies whether a form or an input field should have autocomplete on or off.
    static member inline AutoComplete (value  : bool) = Attribute("autocomplete", AttributeValue.String (if value then "on" else "off"))
    
    /// When present, it specifies that the element should automatically get focus when the page loads.
    static member inline AutoFocus (value  : bool) = Attribute("autofocus", AttributeValue.Bool value)
    
    /// When present, the audio/video will automatically start playing as soon as it can do so without stopping.
    static member inline AutoPlay (value  : bool) = Attribute("autoplay", AttributeValue.Bool value)

    /// When used by the <script> element, the charset attribute specifies the character encoding used in an external script file.
    static member inline Charset (charset : string) = Attribute("charset", AttributeValue.String charset)

    /// specifies that an <input> element should be selected (checked).
    static member inline Checked (isChecked : bool) = Attribute("checked", AttributeValue.Bool isChecked)

    /// The cite attribute specifies a URL to a document that explains the quote, or why the text was inserted/changed.
    static member inline Cite (url : string) = Attribute("cite", AttributeValue.String url)
    
    /// The HTML class attribute is used to specify a class for an HTML element.
    static member inline Class(cls : string) = Attribute("classList", AttributeValue.Set (Set.singleton cls))

    /// The HTML class attribute is used to specify a class for an HTML element.
    static member inline Class(cls : #seq<string>) = Attribute("classList", AttributeValue.Set (Set.ofSeq cls))

    /// The cols attribute specifies the visible width of a text area.
    static member inline Cols(count : int) = Attribute("cols", AttributeValue.Int count)
    
    /// The rows attribute specifies the visible height of a text area.
    static member inline Rows(count : int) = Attribute("rows", AttributeValue.Int count)

    /// The colspan attribute defines the number of columns a table cell should span.
    static member inline ColSpan(count : int) = Attribute("colspan", AttributeValue.Int count)
    
    /// The rowspan attribute defines the number of rows a table cell should span.
    static member inline RowSpan(count : int) = Attribute("rowspan", AttributeValue.Int count)
    
    /// The contenteditable attribute specifies whether the content of an element is editable or not.
    static member inline ContentEditable(editable : bool) = Attribute("contentEditable", AttributeValue.Bool editable)

    /// When present, it specifies that audio/video controls should be displayed.
    static member inline Controls(controls : bool) = Attribute("controls", AttributeValue.Bool controls)
    
    /// The coords attribute specifies the coordinates of an area in an image map.
    static member inline Coords(value : string) = Attribute("coords", AttributeValue.String value)

    /// The datetime attribute specifies the date and time when the text was deleted/inserted.
    static member inline DateTime(value : string) = Attribute("dateTime", AttributeValue.String value)

    /// The datetime attribute specifies the date and time when the text was deleted/inserted.
    static member inline DateTime(value : System.DateTimeOffset) = Attribute("dateTime", AttributeValue.String (value.ToString("o", System.Globalization.CultureInfo.InvariantCulture)))

    /// The datetime attribute specifies the date and time when the text was deleted/inserted.
    static member inline DateTime(value : System.DateTime) = Attribute("dateTime", AttributeValue.String (value.ToString("o", System.Globalization.CultureInfo.InvariantCulture)))
    
    /// When present, it specifies that the track is to be enabled if the user's preferences do not indicate that another track would be more appropriate.
    static member inline Default(value : bool) = Attribute("default", AttributeValue.Bool value)

    /// When present, it specifies that the script is executed when the page has finished parsing.
    static member inline Defer(value : bool) = Attribute("defer", AttributeValue.Bool value)

    /// The dir attribute specifies the text direction of the element's content.
    static member inline Dir(textDirection: string) = Attribute("dir", AttributeValue.String textDirection)

    /// The dirname attribute enables the submission of the text direction of the input field/textarea
    static member inline DirName(name : string) = Attribute("dirName", AttributeValue.String name)

    /// When present, it specifies that the element should be disabled.
    static member inline Disabled(value : bool) = Attribute("disabled", AttributeValue.Bool value)

    /// The download attribute specifies that the target will be downloaded when a user clicks on the hyperlink.
    static member inline Download(name : string) = Attribute("download", AttributeValue.String name)

    /// The draggable attribute specifies whether an element is draggable or not.
    static member inline Draggable(draggable : bool) = Attribute("draggable", AttributeValue.Bool draggable)

    /// The enctype attribute specifies how the form-data should be encoded when submitting it to the server.
    static member inline EncType(name : string) = Attribute("encType", AttributeValue.String name)

    /// When used together with the <label> element, the for attribute specifies which form element a label is bound to.
    static member inline For(name : string) = Attribute("for", AttributeValue.String name)

    /// The form attribute specifies the form the element belongs to.
    static member inline Form(id : string) = Attribute("form", AttributeValue.String id)

    /// The formaction attribute specifies where to send the form-data when a form is submitted. This attribute overrides the form's action attribute.
    static member inline FormAction(action : string) = Attribute("formAction", AttributeValue.String action)

    /// The headers attribute specifies one or more header cells a table cell is related to.
    static member inline Headers(name : string) = Attribute("headers", AttributeValue.String name)

    /// The height attribute specifies the height of the element, in pixels.
    static member inline Height(valueInPx : int) = Attribute("height", AttributeValue.Int valueInPx)
    
    /// The width attribute specifies the width of the element, in pixels.
    static member inline Width(valueInPx : int) = Attribute("width", AttributeValue.Int valueInPx)

    /// When present, it specifies that an element is not yet, or is no longer, relevant.
    static member inline Hidden(hidden : bool) = Attribute("hidden", AttributeValue.Bool hidden)

    /// The high attribute specifies the range where the gauge's value is considered to be a high value.
    static member inline High(value : int) = Attribute("high", AttributeValue.Int value)
    
    /// The low attribute specifies the range where the gauge's value is considered to be a low value.
    static member inline Low(value : int) = Attribute("low", AttributeValue.Int value)
    
    /// The min attribute specifies the minimum value of an element element.
    static member inline Min(value : int) = Attribute("min", AttributeValue.Int value)

    /// The max attribute specifies the maximum value of the element.
    static member inline Max(value : int) = Attribute("max", AttributeValue.Int value)

    /// For <a> and <area> elements, the href attribute specifies the URL of the page the link goes to.
    static member inline Href(url : string) = Attribute("href", AttributeValue.String url)

    /// The hreflang attribute specifies the language of the linked document.
    static member inline HrefLang(lang : string) = Attribute("hrefLang", AttributeValue.String lang)

    /// The HTML id attribute is used to specify a unique id for an HTML element.
    /// You cannot have more than one element with the same id in an HTML document.
    static member inline Id(id : string) = Attribute("id", AttributeValue.String id)
    
    /// The HTML id attribute is used to specify a unique id for an HTML element.
    /// You cannot have more than one element with the same id in an HTML document.
    static member inline Id(id : Guid) = Attribute("id", AttributeValue.String (string id))

    /// When present, it specifies that the image is part of a server-side image map (an image map is an image with clickable areas).
    static member inline IsMap(value : bool) = Attribute("isMap", AttributeValue.Bool value)

    /// The kind attribute specifies the kind of text track.
    static member inline Kind(kind : string) = Attribute("kind", AttributeValue.String kind)

    /// The label attribute specifies the title of the text track.
    static member inline Label(label : string) = Attribute("label", AttributeValue.String label)
    
    /// The lang attribute specifies the language of the element's content.
    static member inline Lang(lang : string) = Attribute("lang", AttributeValue.String lang)

    /// The list attribute refers to a <datalist> element that contains pre-defined options for an <input> element.
    static member inline List(name : string) = Attribute("list", AttributeValue.String name)

    /// When present, it specifies that the audio will start over again, every time it is finished.
    static member inline Loop(value : bool) = Attribute("loop", AttributeValue.Bool value)

    /// The maxlength attribute specifies the maximum number of characters allowed in the element.
    static member inline MaxLength(value : int) = Attribute("maxLength", AttributeValue.Int value)

    /// The media attribute specifies what media/device the linked document is optimized for.
    static member inline Media(value : string) = Attribute("media", AttributeValue.String value)

    /// The method attribute specifies how to send form-data (the form-data is sent to the page specified in the action attribute).
    static member inline Method(meth : string) = Attribute("method", AttributeValue.String meth)

    /// When present, it specifies that the user is allowed to enter/select more than one value.
    static member inline Multiple(value : bool) = Attribute("multiple", AttributeValue.Bool value)

    /// When present, it specifies that the audio output of the video should be muted.
    static member inline Muted(value : bool) = Attribute("muted", AttributeValue.Bool value)

    /// The name attribute specifies a name for an HTML element.
    static member inline Name(name : string) = Attribute("name", AttributeValue.String name)

    /// When present, it specifies that the form-data (input) should not be validated when submitted.
    static member inline NoValidate(value : bool) = Attribute("noValidate", AttributeValue.Bool value)

    /// When present, it specifies that the details should be visible (open) by default.
    static member inline Open(value : bool) = Attribute("open", AttributeValue.Bool value)

    /// The optimum attribute specifies the range where the gauge's value is considered to be an optimal value.
    static member inline Optimum(value : int) = Attribute("optimum", AttributeValue.Int value)

    /// The pattern attribute specifies a regular expression that the <input> element's value is checked against.
    static member inline Pattern(value : string) = Attribute("pattern", AttributeValue.String value)

    /// The placeholder attribute specifies a short hint that describes the expected value of a input field / textarea.
    static member inline Placeholder(value : string) = Attribute("placeholder", AttributeValue.String value)

    /// The poster attribute specifies an image to be shown while the video is downloading, or until the user hits the play button. If this is not included, the first frame of the video will be used instead.
    static member inline Poster(url : string) = Attribute("poster", AttributeValue.String url)

    /// The preload attribute specifies if and how the author thinks that the media file should be loaded when the page loads.
    static member inline Preload(value : string) = Attribute("preload", AttributeValue.String value)

    /// When present, it specifies that an input field or textarea is read-only.
    static member inline Readonly(value : bool) = Attribute("readonly", AttributeValue.Bool value)

    /// The rel attribute specifies the relationship between the current document and the linked document/resource.
    static member inline Rel(value : string) = Attribute("rel", AttributeValue.String value)

    /// When present, it specifies that the element must be filled out before submitting the form.
    static member inline Required(value : bool) = Attribute("required", AttributeValue.Bool value)

    /// When present, it specifies that the list order should be descending (9,8,7...), instead of ascending (1, 2, 3...).
    static member inline Reversed(value : bool) = Attribute("reversed", AttributeValue.Bool value)

    /// The sandbox attribute enables an extra set of restrictions for the content in an iframe.
    static member inline Sandbox(value : string) = Attribute("sandbox", AttributeValue.String value)

    /// The scope attribute specifies whether a header cell is a header for a column, row, or group of columns or rows.
    static member inline Scope(name : string) = Attribute("scope", AttributeValue.String name)

    /// When present, it specifies that an option should be selected.
    static member inline Selected(value : bool) = Attribute("selected", AttributeValue.Bool value)

    /// The shape attribute specifies the shape of an area.
    static member inline Shape(value : string) = Attribute("shape", AttributeValue.String value)

    /// For input elements, the size attribute specifies the visible width, in characters, of an <input> element.
    static member inline Size(value : int) = Attribute("size", AttributeValue.Int value)

    /// The span attribute defines the number of columns a <col> or <colgroup> element should span.
    static member inline Span(value : int) = Attribute("span", AttributeValue.Int value)

    /// The spellcheck attribute specifies whether the element is to have its spelling and grammar checked or not.
    static member inline SpellCheck(value : bool) = Attribute("spellCheck", AttributeValue.Bool value)

    /// The src attribute specifies the location (URL) of the external resource.
    static member inline Src(url : string) = Attribute("src", AttributeValue.String url)
    
    /// The srcdoc attribute specifies the HTML content of the page to show in the inline frame.
    static member inline SrcDoc(content : string) = Attribute("srcDoc", AttributeValue.String content)
    
    /// The srclang attribute specifies the language of the track text data.
    static member inline SrcLang(lang : string) = Attribute("srcLang", AttributeValue.String lang)

    /// The start attribute specifies the start value of the first list item in an ordered list.
    static member inline Start(value : int) = Attribute("start", AttributeValue.Int value)

    /// The step attribute specifies the legal number intervals for an <input> element.
    static member inline Step(value : int) = Attribute("step", AttributeValue.Int value)

    /// The style attribute specifies an inline style for an element.
    /// The style attribute will override any style set globally, e.g. styles specified in the <style> tag or in an external style sheet.
    static member inline Style(style : string) = Attribute("style", AttributeValue.String style)
    
    /// The style attribute specifies an inline style for an element.
    /// The style attribute will override any style set globally, e.g. styles specified in the <style> tag or in an external style sheet.
    static member inline Style(props : #seq<StyleProperty>) =
        let style = props |> Seq.map (fun p -> $"{p.Name}: {p.Value}") |> String.concat "; "
        Attribute("style", AttributeValue.String style)

    /// The tabindex attribute specifies the tab order of an element (when the "tab" button is used for navigating).
    static member inline TabIndex(index : int) = Attribute("tabIndex", AttributeValue.Int index)
    
    /// For <a> and <area> elements, the target attribute specifies where to open the linked document.
    static member inline Target(value : string) = Attribute("target", AttributeValue.String value)

    /// The title attribute specifies extra information about an element.
    static member inline Title(name : string) = Attribute("title", AttributeValue.String name)

    /// The translate attribute specifies whether the content of an element should be translated or not.
    static member inline Translate(value : bool) = Attribute("translate", AttributeValue.Bool value)

    /// For <button> elements, the type attribute specifies the type of button.
    static member inline Type(typ : string) = Attribute("type", AttributeValue.String typ)
    
    static member inline Role(role : string) = Attribute("role", AttributeValue.String role)
    
    /// The usemap attribute specifies an image (or an object) as an image map (an image map is an image with clickable areas).
    static member inline UseMap(name : string) = Attribute("useMap", AttributeValue.String name)

    /// For <button>, <input> and <option> elements, the value attribute specifies the initial value of the element.
    static member inline Value(value : string) = Attribute("value", AttributeValue.String value)
    
    /// For <button>, <input> and <option> elements, the value attribute specifies the initial value of the element.
    static member inline Value(value : int) = Attribute("value", AttributeValue.Int value)
    
    /// For <button>, <input> and <option> elements, the value attribute specifies the initial value of the element.
    static member inline Value(value : bool) = Attribute("value", AttributeValue.Bool value)

    /// The wrap attribute specifies how the text in a text area is to be wrapped when submitted in a form.
    static member inline Wrap(value : string) = Attribute("wrap", AttributeValue.String value)


    static member inline On<'a when 'a :> Event>(name : string, callback : 'a -> bool, ?useCapture : bool, ?pointerCapture : bool, ?preventDefault : bool) =
        let pointerCapture = defaultArg pointerCapture false
        let preventDefault = defaultArg preventDefault false
        match useCapture with
        | Some true -> 
            Attribute(name, AttributeValue.Event({ EventType = typeof<'a>; Callback = Some (unbox<'a> >> callback); PointerCapture = pointerCapture; PreventDefault = preventDefault}, EventCode.Empty))
        | _ -> 
            Attribute(name, AttributeValue.Event(EventCode.Empty, { EventType = typeof<'a>; Callback = Some (unbox<'a> >> callback); PointerCapture = pointerCapture; PreventDefault = preventDefault }))

            
    static member inline On<'a when 'a :> Event>(name : string, callback : 'a -> unit, ?useCapture : bool, ?pointerCapture : bool, ?preventDefault : bool) =
        let pointerCapture = defaultArg pointerCapture false
        let preventDefault = defaultArg preventDefault false
        match useCapture with
        | Some true -> 
            Attribute(name, AttributeValue.Event({ EventType = typeof<'a>; Callback = Some (fun e -> callback (unbox e); true); PointerCapture = pointerCapture; PreventDefault = preventDefault}, EventCode.Empty))
        | _ -> 
            Attribute(name, AttributeValue.Event(EventCode.Empty, { EventType = typeof<'a>; Callback = Some (fun e -> callback (unbox e); true); PointerCapture = pointerCapture; PreventDefault = preventDefault }))

    static member inline OnContextMenu(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "contextmenu", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnContextMenu(callback : MouseEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "contextmenu", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )

    static member inline OnMouseEnter(callback : MouseEvent -> unit) = 
        Dom.On(
            "mouseenter", 
            callback
        )
        
    static member inline OnMouseLeave(callback : MouseEvent -> unit) = 
        Dom.On(
            "mouseleave", 
            callback
        )

    static member inline OnClick(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "click", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnClick(callback : MouseEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "click", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )

    static member inline OnDoubleClick(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "dblclick", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
    
    static member inline OnDoubleClick(callback : MouseEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "dblclick", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
    
    static member inline OnMouseDown(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "mousedown", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnMouseDown(callback : MouseEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "mousedown", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )

    static member inline OnMouseUp(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "mouseup", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnMouseUp(callback : MouseEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "mouseup", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )

    static member inline OnMouseMove(callback : MouseEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "mousemove", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
    
    static member inline OnMouseMove(callback : MouseEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "mousemove", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
    
    static member inline OnMouseWheel(callback : WheelEvent -> unit, ?useCapture : bool, ?preventDefault : bool) =
        Dom.On(
            "wheel", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )

    static member inline OnMouseWheel(callback : WheelEvent -> bool, ?useCapture : bool, ?preventDefault : bool) =
        Dom.On(
            "wheel", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        

    static member inline OnPointerDown(callback : PointerEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?pointerCapture : bool) = 
        Dom.On(
            "pointerdown", 
            callback, 
            ?pointerCapture = pointerCapture,
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnPointerDown(callback : PointerEvent -> bool, ?useCapture : bool, ?preventDefault : bool, ?pointerCapture : bool) = 
        Dom.On(
            "pointerdown", 
            callback, 
            ?pointerCapture = pointerCapture,
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        

    static member inline OnPointerUp(callback : PointerEvent -> unit, ?useCapture : bool, ?preventDefault : bool, ?pointerCapture : bool) = 
        Dom.On(
            "pointerup", 
            callback,
            ?pointerCapture = pointerCapture,
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnPointerUp(callback : PointerEvent -> bool, ?useCapture : bool, ?preventDefault : bool, ?pointerCapture : bool) = 
        Dom.On(
            "pointerup", 
            callback,
            ?pointerCapture = pointerCapture,
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        

    static member inline OnPointerMove(callback : PointerEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "pointermove", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnPointerMove(callback : PointerEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "pointermove", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )

    static member inline OnTouchStart(callback : TouchEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "touchstart", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnTouchStart(callback : TouchEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "touchstart", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
    

    static member inline OnTouchEnd(callback : TouchEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "touchend", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnTouchEnd(callback : TouchEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "touchend", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnTouchCancel(callback : TouchEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "touchcancel", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnTouchCancel(callback : TouchEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "touchcancel", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnTouchMove(callback : TouchEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "touchmove", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnTouchMove(callback : TouchEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "touchmove", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
    
    static member inline OnKeyDown(callback : KeyboardEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "keydown", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
            
    static member inline OnKeyDown(callback : KeyboardEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "keydown", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
           
    static member inline OnKeyUp(callback : KeyboardEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "keyup", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
            
    static member inline OnKeyUp(callback : KeyboardEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "keyup", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnTap(callback : TapEvent -> bool, ?useCapture : bool, ?preventDefault : bool) =
        Dom.On(
            "tap", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnTap(callback : TapEvent -> unit, ?useCapture : bool, ?preventDefault : bool) =
        Dom.On(
            "tap", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )

        
    static member inline OnDoubleTap(callback : TapEvent -> unit, ?useCapture : bool, ?preventDefault : bool) =
        Dom.On(
            "dbltap", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
        
    static member inline OnDoubleTap(callback : TapEvent -> bool, ?useCapture : bool, ?preventDefault : bool) =
        Dom.On(
            "dbltap", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnLongPress(callback : PointerEvent -> unit, ?useCapture : bool, ?preventDefault : bool) =
        Dom.On(
            "longpress", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnLongPress(callback : PointerEvent -> bool, ?useCapture : bool, ?preventDefault : bool) =
        Dom.On(
            "longpress", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )

    static member inline Require(url : string) = 
        Attribute("require", AttributeValue.Set (Set.singleton url))

    static member inline Require(urls : #seq<string>) =
        Attribute("require", AttributeValue.Set (Set.ofSeq urls))

    static member inline OnBoot(code : string) =
        Attribute("boot", AttributeValue.Execute([||], fun _ -> [code]))

    static member inline OnBoot(channel : IChannel -> Task<unit>, js : string -> string) =
        Attribute("boot", AttributeValue.Execute([|channel|], fun n -> [js n.[0]]))

    static member inline OnShutdown(code : string) =
        Attribute("shutdown", AttributeValue.Execute([||], fun _ -> [code]))

    static member inline OnBoot(code : #seq<string>) =
        Attribute("boot", AttributeValue.Execute([||], fun _ -> [String.concat "\n" code]))

    static member inline OnShutdown(code : #seq<string>) =
        Attribute("shutdown", AttributeValue.Execute([||], fun _ -> [String.concat "\n" code]))
        
    static member OnChange(callback : ChangeEvent -> bool, ?preventDefault : bool) =
        Dom.On(
            "change", 
            callback, 
            ?preventDefault = preventDefault
        )
            
    static member OnChange(callback : ChangeEvent -> unit, ?preventDefault : bool) =
        Dom.On(
            "change", 
            callback, 
            ?preventDefault = preventDefault
        )
    static member OnInput(callback : InputEvent -> bool, ?preventDefault : bool) =
        Dom.On(
            "input", 
            callback, 
            ?preventDefault = preventDefault
        )
            
    static member OnInput(callback : InputEvent -> unit, ?preventDefault : bool) =
        Dom.On(
            "input", 
            callback, 
            ?preventDefault = preventDefault
        )
        
    static member OnBeforeInput(callback : InputEvent -> bool, ?preventDefault : bool) =
        Dom.On(
            "beforeinput", 
            callback, 
            ?preventDefault = preventDefault
        )
            
    static member OnBeforeInput(callback : InputEvent -> unit, ?preventDefault : bool) =
        Dom.On(
            "beforeinput", 
            callback, 
            ?preventDefault = preventDefault
        )


    static member inline OnGamepadAxisChange(callback : GamepadAxisChangeEvent -> unit, ?useCapture : bool, ?preventDefault : bool) =
        Dom.On(
            "gamepadaxischange", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnGamepadAxisChange(callback : GamepadAxisChangeEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "gamepadaxischange", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnGamepadButtonDown(callback : GamepadButtonEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "gamepadbuttondown", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnGamepadButtonDown(callback : GamepadButtonEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "gamepadbuttondown", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnGamepadButtonUp(callback : GamepadButtonEvent -> unit, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "gamepadbuttonup", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )
        
    static member inline OnGamepadButtonUp(callback : GamepadButtonEvent -> bool, ?useCapture : bool, ?preventDefault : bool) = 
        Dom.On(
            "gamepadbuttonup", 
            callback, 
            ?useCapture = useCapture,
            ?preventDefault = preventDefault
        )



[<AbstractClass; Sealed; AutoOpen>]
type RenderControl private() =
    static member inline Samples(samples : int) =
        Attribute("data-samples", AttributeValue.Int samples)
        
    static member inline Quality(quality : int) =
        Attribute("data-quality", AttributeValue.Int quality)

    static member inline OnResize (action : RenderControlEventInfo -> unit) =   
        RenderControlEventKind.Resize, fun (e : RenderControlEventInfo) -> action e
        
    static member inline OnBeforeRender (action : RenderControlEventInfo -> unit) =   
        RenderControlEventKind.PreRender, fun (e : RenderControlEventInfo) -> action e

    static member inline OnRendered (action : RenderControlEventInfo -> unit) =   
        RenderControlEventKind.PostRender, fun (e : RenderControlEventInfo) -> action e

[<AbstractClass; Sealed; AutoOpen>]
type Css private() =
    static member inline PointerEvents(value : string) = StyleProperty("pointer-events", value)
    static member inline UserSelect(value : string) = StyleProperty("user-select", value)
    static member inline WebkitUserSelect(value : string) = StyleProperty("-webkit-user-select", value)
    static member inline Overscroll(value : string) = StyleProperty("overscroll", value)
    static member inline BackdropFilter(value : string) = StyleProperty("backdrop-filter", value)
    static member inline WebkitBackdropFilter(value : string) = StyleProperty("-webkit-backdrop-filter", value)
    static member inline AlignItems(value : string) = StyleProperty("align-items", value)
    static member inline JustifyContent(value : string) = StyleProperty("justify-content", value)
    static member inline Color(value : string) = StyleProperty("color", value)
    static member inline Cursor(value : string) = StyleProperty("cursor", value)
    static member inline FontFamily(value : string) = StyleProperty("font-family", value)
    static member inline FontSize(value : string) = StyleProperty("font-size", value)
    static member inline FontStyle(value : string) = StyleProperty("font-style", value)
    static member inline FontWeight(value : string) = StyleProperty("font-weight", value)
    static member inline ListStyleType(value : string) = StyleProperty("list-style-type", value)
    static member inline TextDecoration(value : string) = StyleProperty("text-decoration", value)
    static member inline TextAlign(value : string) = StyleProperty("text-align", value)
    static member inline VerticalAlign(value : string) = StyleProperty("vertical-align", value)
    static member inline Display(value : string) = StyleProperty("display", value)
    static member inline Position(value : string) = StyleProperty("position", value)
    static member inline Float(value : string) = StyleProperty("float", value)
    static member inline Clear(value : string) = StyleProperty("clear", value)
    static member inline Margin(value : string) = StyleProperty("margin", value)
    static member inline MarginTop(value : string) = StyleProperty("margin-top", value)
    static member inline MarginRight(value : string) = StyleProperty("margin-right", value)
    static member inline MarginBottom(value : string) = StyleProperty("margin-bottom", value)
    static member inline MarginLeft(value : string) = StyleProperty("margin-left", value)
    static member inline Padding(value : string) = StyleProperty("padding", value)
    static member inline PaddingTop(value : string) = StyleProperty("padding-top", value)
    static member inline PaddingRight(value : string) = StyleProperty("padding-right", value)
    static member inline PaddingBottom(value : string) = StyleProperty("padding-bottom", value)
    static member inline PaddingLeft(value : string) = StyleProperty("padding-left", value)
    static member inline Border(value : string) = StyleProperty("border", value)
    static member inline BorderTop(value : string) = StyleProperty("border-top", value)
    static member inline BorderRight(value : string) = StyleProperty("border-right", value)
    static member inline BorderBottom(value : string) = StyleProperty("border-bottom", value)
    static member inline BorderLeft(value : string) = StyleProperty("border-left", value)
    static member inline BorderWidth(value : string) = StyleProperty("border-width", value)
    static member inline BorderTopWidth(value : string) = StyleProperty("border-top-width", value)
    static member inline BorderRightWidth(value : string) = StyleProperty("border-right-width", value)
    static member inline BorderBottomWidth(value : string) = StyleProperty("border-bottom-width", value)
    static member inline BorderLeftWidth(value : string) = StyleProperty("border-left-width", value)
    static member inline BorderStyle(value : string) = StyleProperty("border-style", value)
    static member inline BorderTopStyle(value : string) = StyleProperty("border-top-style", value)
    static member inline BorderRightStyle(value : string) = StyleProperty("border-right-style", value)
    static member inline BorderBottomStyle(value : string) = StyleProperty("border-bottom-style", value)
    static member inline BorderLeftStyle(value : string) = StyleProperty("border-left-style", value)
    static member inline BorderColor(value : string) = StyleProperty("border-color", value)
    static member inline BorderTopColor(value : string) = StyleProperty("border-top-color", value)
    static member inline BorderRightColor(value : string) = StyleProperty("border-right-color", value)
    static member inline BorderBottomColor(value : string) = StyleProperty("border-bottom-color", value)
    static member inline BorderLeftColor(value : string) = StyleProperty("border-left-color", value)
    static member inline BorderRadius(value : string) = StyleProperty("border-radius", value)
    static member inline BorderTopLeftRadius(value : string) = StyleProperty("border-top-left-radius", value)
    static member inline BorderTopRightRadius(value : string) = StyleProperty("border-top-right-radius", value)
    static member inline BorderBottomLeftRadius(value : string) = StyleProperty("border-bottom-left-radius", value)
    static member inline BorderBottomRightRadius(value : string) = StyleProperty("border-bottom-right-radius", value)
    static member inline Width(value : string) = StyleProperty("width", value)
    static member inline Height(value : string) = StyleProperty("height", value)
    static member inline MinWidth(value : string) = StyleProperty("min-width", value)
    static member inline MinHeight(value : string) = StyleProperty("min-height", value)
    static member inline MaxWidth(value : string) = StyleProperty("max-width", value)
    static member inline MaxHeight(value : string) = StyleProperty("max-height", value)
    static member inline Overflow(value : string) = StyleProperty("overflow", value)
    static member inline OverflowX(value : string) = StyleProperty("overflow-x", value)
    static member inline OverflowY(value : string) = StyleProperty("overflow-y", value)
    static member inline Visibility(value : string) = StyleProperty("visibility", value)
    static member inline Top(value : string) = StyleProperty("top", value)
    static member inline Right(value : string) = StyleProperty("right", value)
    static member inline Bottom(value : string) = StyleProperty("bottom", value)
    static member inline Left(value : string) = StyleProperty("left", value)
    static member inline Clip(value : string) = StyleProperty("clip", value)
    static member inline ClipTop(value : string) = StyleProperty("clip-top", value)
    static member inline ClipRight(value : string) = StyleProperty("clip-right", value)
    static member inline ClipBottom(value : string) = StyleProperty("clip-bottom", value)
    static member inline ClipLeft(value : string) = StyleProperty("clip-left", value)
    static member inline Opacity(value : string) = StyleProperty("opacity", value)
    static member inline Filter(value : string) = StyleProperty("filter", value)
    static member inline ZIndex(value : int) = StyleProperty("z-index", string value)
    static member inline Background(value : string) = StyleProperty("background", value)
    static member inline BackgroundColor(value : string) = StyleProperty("background-color", value)
    static member inline BackgroundImage(value : string) = StyleProperty("background-image", value)
    static member inline BackgroundPosition(value : string) = StyleProperty("background-position", value)
    static member inline BackgroundRepeat(value : string) = StyleProperty("background-repeat", value)
    static member inline BackgroundAttachment(value : string) = StyleProperty("background-attachment", value)
    static member inline BackgroundSize(value : string) = StyleProperty("background-size", value)
    static member inline BackgroundOrigin(value : string) = StyleProperty("background-origin", value)
    static member inline BackgroundClip(value : string) = StyleProperty("background-clip", value)
    static member inline Outline(value : string) = StyleProperty("outline", value)
    
    static member inline FontSize(value : int) =
        StyleProperty("font-size", sprintf "%dpx" value)

    static member inline FontSize(value : float) =
        StyleProperty("font-size", sprintf "%fpx" value)

    static member inline Color(c : C3b) = 
        StyleProperty("color", sprintf "#%02X%02X%02X" c.R c.G c.B)

    static member inline Color(c : C4b) = 
        if c.A = 255uy then StyleProperty("color", sprintf "#%02X%02X%02X" c.R c.G c.B)
        else StyleProperty("color", sprintf "rgba(%d, %d, %d, %f)"  c.R c.G c.B (float c.A / 255.0))

    static member inline Background(c : C3b) = 
        StyleProperty("background", sprintf "#%02X%02X%02X" c.R c.G c.B)
        
    static member inline Background(c : C4b) = 
        if c.A = 255uy then StyleProperty("background", sprintf "#%02X%02X%02X" c.R c.G c.B)
        else StyleProperty("background", sprintf "rgba(%d, %d, %d, %f)"  c.R c.G c.B (float c.A / 255.0))

    static member inline Color(c : C4f) =
        let r = (if c.R < 0.0f then 0.0f elif c.R > 1.0f then 1.0f else c.R) * 255.0f |> round |> int
        let g = (if c.G < 0.0f then 0.0f elif c.G > 1.0f then 1.0f else c.G) * 255.0f |> round |> int
        let b = (if c.B < 0.0f then 0.0f elif c.B > 1.0f then 1.0f else c.B) * 255.0f |> round |> int
        if c.A >= 1.0f then StyleProperty("color", sprintf "#%02X%02X%02X" r g b)
        else StyleProperty("color", sprintf "rgba(%d, %d, %d, %.5f)" r g b (max 0.0f c.A))

    static member inline Color(c : C3f) =
        let r = (if c.R < 0.0f then 0.0f elif c.R > 1.0f then 1.0f else c.R) * 255.0f |> round |> int
        let g = (if c.G < 0.0f then 0.0f elif c.G > 1.0f then 1.0f else c.G) * 255.0f |> round |> int
        let b = (if c.B < 0.0f then 0.0f elif c.B > 1.0f then 1.0f else c.B) * 255.0f |> round |> int
        StyleProperty("background", sprintf "#%02X%02X%02X" r g b)

    static member inline Color(r : float, g : float, b : float, a : float) = 
        let r = (if r < 0.0 then 0.0 elif r > 1.0 then 1.0 else r) * 255.0 |> round |> int
        let g = (if g < 0.0 then 0.0 elif g > 1.0 then 1.0 else g) * 255.0 |> round |> int
        let b = (if b < 0.0 then 0.0 elif b > 1.0 then 1.0 else b) * 255.0 |> round |> int
        if a >= 1.0 then StyleProperty("background", sprintf "#%02X%02X%02X" r g b)
        else StyleProperty("color", sprintf "rgba(%d, %d, %d, %.5f)" r g b (max 0.0 a))

    static member inline Color(r : float, g : float, b : float) =
        Css.Color(r, g, b, 1.0)
        
    static member inline Background(r : float, g : float, b : float, a : float) = 
        let r = (if r < 0.0 then 0.0 elif r > 1.0 then 1.0 else r) * 255.0 |> round |> int
        let g = (if g < 0.0 then 0.0 elif g > 1.0 then 1.0 else g) * 255.0 |> round |> int
        let b = (if b < 0.0 then 0.0 elif b > 1.0 then 1.0 else b) * 255.0 |> round |> int
        if a >= 1.0 then StyleProperty("background", sprintf "#%02X%02X%02X" r g b)
        else StyleProperty("background", sprintf "rgba(%d, %d, %d, %.5f)" r g b (max 0.0 a))

    static member inline Background(r : float, g : float, b : float) =
        Css.Background(r, g, b, 1.0)
        