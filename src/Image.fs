namespace wordcloud

open System
open System.IO
open System.Xml
open SkiaSharp
open Constants
open Utils
open Extensions

module Images =
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

    type ImageSize =
    | MobileSmall
    | MobileMedium
    | HDStandard720p
    | HDFull1080p
    | HDUltra4K
    | A4
    | Poster11x17
    | Poster18x24
    | Poster24x36

    let SizeMap =
        [ MobileSmall, SKSizeI(480, 800)
          MobileMedium, SKSizeI(640, 1146)
          HDStandard720p, SKSizeI(1280, 720)
          HDFull1080p, SKSizeI(1920, 1080)
          HDUltra4K, SKSizeI(3840, 2160)
          A4, SKSizeI(816, 1056)
          Poster11x17, SKSizeI(1056, 1632)
          Poster18x24, SKSizeI(1728, 2304)
          Poster24x36, SKSizeI(2304, 3456) ]
        |> Map.ofList