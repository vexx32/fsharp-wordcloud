namespace wordcloud

open System
open SkiaSharp
open Utils
open Extensions

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

    let private getRectanglePath (rectangle: SKRect) =
        let path = new SKPath()
        let radius = rectangle.Height / 16f
        use bubble = new SKRoundRect(rectangle, radius)
        path.AddRoundRect bubble

        path

    let private getSquarePath (rectangle: SKRect) =
        let path = new SKPath()
        let radius = Math.Max(rectangle.Width, rectangle.Height) / 16f
        use bubble = new SKRoundRect(rectangle |> getEnclosingSquare, radius)
        path.AddRoundRect bubble

        path

    let private getCirclePath (rectangle: SKRect) =
        let path = new SKPath()
        let radius = Math.Max(rectangle.Width, rectangle.Height) / 2f
        path.AddCircle(x = rectangle.MidX, y = rectangle.MidY, radius = radius)

        path

    let private getOvalPath (rectangle: SKRect) =
        let padding = float32 (Math.Sqrt <| float (rectangle.Width * rectangle.Height)) / 10f
        let path = new SKPath()
        SKRect.Inflate(rectangle, x = padding, y = padding)
        |> path.AddOval

        path
        
    let getWordBubble shape (word: Word) =
        match word.Path with
        | Some path -> 
            let bounds = SKRect.Inflate(word.Bounds, word.Padding, word.Padding)
            match shape with
            | Some Rectangle -> getRectanglePath bounds |> Some
            | Some Square -> getSquarePath bounds |> Some
            | Some Circle -> getCirclePath bounds |> Some
            | Some Oval -> getOvalPath bounds |> Some
            | None -> None
        | None -> None

    let private wordRectFits rectangle padding (image: Image) =
        let paddedRect = SKRect.Inflate(rectangle, x = padding, y = padding)
        not (intersectsRectangle image.OccupiedSpace rectangle)

    let private getPaddedPath padding (path: SKPath)  =
        let scale = SKMatrix.CreateScale(x = 1f + padding / path.TightBounds.Width, y = 1f + padding / path.TightBounds.Height, pivotX = path.TightBounds.MidX, pivotY = path.TightBounds.MidY)
        let resultPath = new SKPath()
        path.Transform(scale, resultPath)
        
        resultPath

    let private wordPathFits (path: SKPath) padding (image: Image) =
        use paddedPath = path |> getPaddedPath padding
        not (intersectsPath image.OccupiedSpace paddedPath)

    let wordWillFit (word: Word) (image: Image) =
        if fallsOutside word.Bounds image.ClippingRegion then
            false
        else
            match word.Bubble with
            | Some bubble -> wordPathFits bubble word.Padding image
            | None -> wordRectFits word.Bounds word.Padding image

        

    


    