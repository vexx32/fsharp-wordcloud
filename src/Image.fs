namespace wordcloud

open System
open System.IO
open System.Xml
open SkiaSharp
open Constants
open Utils
open Extensions

type Image private (size: SKSizeI, backgroundColor: SKColor, allowOverflow: bool) as this =
    let memoryStream = new SKDynamicMemoryWStream()

    let viewbox =
        new SKRect(left = 0f, top = 0f, right = float32 size.Width, bottom = float32 size.Height)

    let clippingRegion = new SKRegion()

    let canvas =
        SKSvgCanvas.Create(viewbox, memoryStream)

    let mutable backgroundColor = backgroundColor

    do
        if allowOverflow then
            let clippingScaleX = viewbox.Width * (BleedAreaScale - 1f)
            let clippingScaleY = viewbox.Height * (BleedAreaScale - 1f)

            SKRect.Inflate(viewbox, clippingScaleX, clippingScaleY)
            |> SKRectI.Round
        else
            viewbox
            |> SKRectI.Round
        |> clippingRegion.SetRect
        |> ignore

        canvas.Clear backgroundColor

    member val Canvas = canvas

    member val OccupiedSpace = new SKRegion()

    member val ClippingRegion = clippingRegion

    member val Centre = new SKPoint(viewbox.MidX, viewbox.MidY)

    member val Viewbox = viewbox

    member val Origin = viewbox.Location

    member val AspectRatio = viewbox.Width / viewbox.Height

    member val MaxDrawRadius = 
        let radius = SKPoint.Distance(this.Origin, this.Centre)
        if allowOverflow then
            radius * BleedAreaScale
        else
            radius

    member private this.backgroundColor
        with get() = backgroundColor
        and set(value) = backgroundColor <- value

    member this.BackgroundColor
        with get () = this.backgroundColor

    member private this.SealCanvas() =
        this.Canvas.Flush()
        this.Canvas.Dispose()
        memoryStream.Flush()

    member private this.EnsureSvgHasViewbox (xml: XmlDocument) =
        let svgElement = xml.GetElementsByTagName("svg").[0]
        match svgElement with
        | :? XmlElement as element -> 
            if element.GetAttribute "viewbox" = String.Empty then
                element.SetAttribute("viewbox", $"{viewbox.Location.X} {viewbox.Location.Y} {viewbox.Width} {viewbox.Height}")
        | _ -> ()

    member this.GetFinalSvg() =
        this.SealCanvas()
        use data = memoryStream.DetachAsData()
        use reader = new StreamReader(data.AsStream(), leaveOpen = false)

        let xml = new XmlDocument()
        reader.ReadToEnd() |> xml.LoadXml
        this.EnsureSvgHasViewbox xml

        xml

    member this.DrawPath (path: SKPath) (brush: SKPaint) =
        this.OccupiedSpace.CombineWithPath path SKRegionOperation.Union
        this.Canvas.DrawPath(path, brush)

    private new(background: SKBitmap, allowOverflow: bool) as this =
        new Image(new SKSizeI(background.Width, background.Height), SKColor.Empty, allowOverflow) 
        then
            try
                this.Canvas.DrawBitmap(background, x = 0f, y = 0f)
                this.backgroundColor <- List.ofArray background.Pixels |> GetAverageColor
            finally
                background.Dispose()

    internal new(backgroundPath: string, allowOverflow: bool) =
        new Image(SKBitmap.Decode backgroundPath, allowOverflow)

    interface IDisposable with
        member this.Dispose() = 
            this.ClippingRegion.Dispose()
            this.OccupiedSpace.Dispose()
            this.Canvas.Dispose()
            memoryStream.Dispose()
