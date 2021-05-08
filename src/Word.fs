namespace wordcloud

open System
open SkiaSharp
open Utils

module Words =
    type Word =
        { Text: string
          Size: float32
          mutable Angle: float32
          mutable Padding: float32
          FocusWord: bool
          mutable Path: option<SKPath>
          mutable Bubble: option<SKPath> }
        
        static member Default =
            { Text = ""
              Size = 0f
              Angle = 0f
              Padding = 0f
              FocusWord = false
              Path = None
              Bubble = None }

        member this.Bounds with get() = 
            match this.Bubble with
            | Some bubble -> bubble.TightBounds
            | None ->
                match this.Path with
                | Some path -> path.TightBounds
                | None -> SKRect.Empty

        override this.ToString() = this.Text

        interface IDisposable with
            member this.Dispose() =
                match this.Path with
                | Some p -> p.Dispose()
                | None -> ()

                match this.Bubble with
                | Some b -> b.Dispose()
                | None -> ()

    let getScaledSize (word: Word) scale = word.Size * scale

    let rotateWord (word: Word) degrees =
        match word.Path with
        | Some path -> 
            word.Angle <- word.Angle + degrees
            path |> rotatePath <| degrees
            
            match word.Bubble with
            | Some path -> path |> rotatePath <| degrees
            | None -> ()
        | None -> ()

    let moveWord (word: Word) point =
        match word.Path with
        | Some path ->
            path |> centrePathOnPoint <| point

            match word.Bubble with
            | Some bubble -> bubble |> centrePathOnPoint <| point
            | None -> ()
        | None -> ()
        


        

    


    