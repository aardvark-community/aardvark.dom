namespace Aardvark.Dom

open System.Runtime.CompilerServices
open System.Net.WebSockets
open System.Threading
open System
open System.Text

[<AbstractClass; Sealed; Extension>]
type WebSocketExtensions private() =
    [<Extension>]
    static member ReceiveMessage(socket : WebSocket, ct : CancellationToken) =
        task {
            let mutable buffer = Array.zeroCreate 4096
            let! res = socket.ReceiveAsync(ArraySegment buffer, ct)
            let typ = res.MessageType
            if res.EndOfMessage then
                if res.Count < buffer.Length then Array.Resize(&buffer, res.Count)
                return typ, buffer
            else
                let mutable size = res.Count
                let mutable fin = false
                while not fin do
                    let rem = buffer.Length - size
                    if rem < 2048 then 
                        let cap = buffer.Length * 2
                        Array.Resize(&buffer, cap)
                    
                    let! res = socket.ReceiveAsync(ArraySegment(buffer, size, buffer.Length - size), ct)
                    size <- size + res.Count
                    if res.EndOfMessage then    
                        fin <- true

                if size < buffer.Length then Array.Resize(&buffer, size)
                return typ, buffer
        }
    
    [<Extension>]
    static member ReceiveMessage(socket : WebSocket) =
        socket.ReceiveMessage(CancellationToken.None)

    [<Extension>]
    static member Send(socket : WebSocket, message : string) =
        let bytes = Encoding.UTF8.GetBytes message
        socket.SendAsync(ArraySegment bytes, WebSocketMessageType.Text, true, CancellationToken.None)
        
    [<Extension>]
    static member Send(socket : WebSocket, bytes : byte[]) =
        socket.SendAsync(ArraySegment bytes, WebSocketMessageType.Binary, true, CancellationToken.None)
