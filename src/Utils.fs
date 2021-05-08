namespace wordcloud

open System
open System.Xml
open System.Text
open SkiaSharp
open Constants

module Utils =

    [<Struct>]
    type WordOrientations = 
    | Vertical
    | FlippedVertical
    | EitherVertical
    | UprightDiagonal
    | InvertedDiagonal
    | AnyDiagonal
    | AnyUpright
    | AnyInverted
    | Any

    [<Struct>]
    type BubbleShape =
    | Rectangle
    | Square
    | Circle
    | Oval

    /// Defines an implicit conversion operator to convert one type to another if an op_Implicit conversion
    /// exists to convert <c>^a</c> to <c>^b</c>.
    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

    let private random = Random()

    let getBrush color wordSize strokeWidth typeface =
        new SKPaint(
            Typeface = typeface,
            TextSize = wordSize,
            Style = SKPaintStyle.StrokeAndFill,
            Color = color,
            StrokeWidth = wordSize * strokeWidth * strokeBaseScale,
            IsStroke = false,
            IsAutohinted = true,
            IsAntialias = true)

    let getPlainBrush = getBrush SKColors.Black

    let averageCharArea (typeface: SKTypeface) =
        let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        use brush = getPlainBrush 1f 0f typeface
        use path = brush.GetTextPath(alphabet, x = 0f, y = 0f)

        let bounds = path.TightBounds
        bounds.Width * bounds.Height / float32 alphabet.Length

    /// <summary>
    /// Gets the angle increment value based on the radius and step values given.
    /// </summary>
    /// <param name="radius">The distance from the centre point.</param>
    /// <param name="step">The radial step increment value.</param>
    /// <returns></returns>
    let getAngleIncrement (radius: float32) step =
        step * baseAngularIncrement
        / (15 * int (Math.Sqrt(float radius)))

    let getAverageColor (colorList: list<SKColor>) =
        let mutable totalRed = 0
        let mutable totalGreen = 0
        let mutable totalBlue = 0
        let mutable totalAlpha = 0

        colorList |> List.iter (fun (color: SKColor) -> 
            totalRed <- totalRed + int color.Red
            totalGreen <- totalGreen + int color.Green
            totalBlue <- totalBlue + int color.Blue
            totalAlpha <- totalAlpha + int color.Alpha)

        let red = byte (totalRed / colorList.Length)
        let green = byte (totalRed / colorList.Length)
        let blue = byte (totalBlue / colorList.Length)
        let alpha = byte (totalBlue / colorList.Length)

        SKColor(red, green, blue, alpha)
        
    let monochrome (color: SKColor) =
        match color.ToHsv() with
        | _, _, brightness -> 
            let level = byte (Math.Floor(255.0 * float brightness / 100.0))
            SKColor(level, level, level)

    let isTransparent (color: SKColor) =
        color.Alpha = 0uy

    let distinctFrom (target: SKColor) (reference: SKColor) =
        if isTransparent target then
            false
        else
            let (refHue, refSaturation, refBrightness) = reference.ToHsv()
            let (hue, saturation, brightness) = target.ToHsv()

            let brightnessDistance = Math.Abs(refBrightness - brightness)
            let hueDistance = Math.Abs(refHue - hue)
            let saturationDistance = Math.Abs(refSaturation - saturation)

            match brightnessDistance, hueDistance, saturationDistance with
            | b, h, s when b > 30f 
                || (b > 20f && h > 24f)
                || (b > 18f && s > 24f) -> true
            | _ -> false

    let getEnclosingSquare (rectangle: SKRect) =
        match rectangle with
        | wide when wide.Width > wide.Height -> SKRect.Inflate(wide, x = 0f, y = (wide.Width - wide.Height) / 2f)
        | tall when tall.Height > tall.Width -> SKRect.Inflate(tall, x = (tall.Height - tall.Width) / 2f, y = 0f)
        | square -> square

    let shuffle (list: list<'a>) =
        let mutable index = list.Length
        let array = List.toArray list

        while (index > 1) do
            let otherIndex = random.Next index
            index <- index - 1

            let temp = array.[index]
            array.[index] <- array.[otherIndex]
            array.[otherIndex] <- temp

        List.ofArray array

    let randomFloat min max =
        if min >= max then
            max
        else
            let range = max - min
            float32 (random.NextDouble()) * range + min
       
    let getRandomFloats min max =
        let minCount = 3
        let maxCount = 17

        [ for _ in minCount .. maxCount -> randomFloat min max ]

    let getRotationAngles orientations =
        match orientations with
        | Some Vertical -> shuffle [0f; 90f]
        | Some FlippedVertical -> shuffle [0f; -90f]
        | Some EitherVertical -> shuffle [0f; 90f; -90f]
        | Some UprightDiagonal -> shuffle [-90f .. 45f .. 90f]
        | Some InvertedDiagonal -> shuffle [90f; 135f; -135f; -90f; 180f]
        | Some AnyDiagonal -> shuffle [45f; 90f; 135f; 180f; -135f; -90f; -45f; 0f]
        | Some AnyUpright -> getRandomFloats -90f 91f
        | Some AnyInverted -> getRandomFloats 90f 271f
        | Some Any -> getRandomFloats 0f 361f
        | None -> [ 0f ]

    let isMostlyVertical angle =
        let remainder = Math.Abs(angle % 180f)
        135f > remainder && remainder > 45f

    let fallsOutside (rectangle: SKRect) (region: SKRegion) =
        let bounds: SKRect = !> region.Bounds
        rectangle.Top < bounds.Top
        || rectangle.Bottom > bounds.Bottom
        || rectangle.Left < bounds.Left
        || rectangle.Right > bounds.Right
    
    let isEmpty (region: SKRegion) = region.Bounds.IsEmpty

    let intersectsRectangle (region: SKRegion) rectangle =
        if isEmpty region then
            false
        else
            rectangle 
            |> SKRectI.Round 
            |> region.Intersects

    let intersectsPath (region: SKRegion) (path: SKPath) =
        if isEmpty region then
            false
        else
            region.Intersects(path)

    let centrePathOnPoint (path: SKPath) (point: SKPoint) =
        let midpoint = new SKPoint(path.TightBounds.MidX, path.TightBounds.MidY)
        path.Offset(point - midpoint)

    let rotatePath (path: SKPath) degrees =
        let bounds = path.TightBounds
        let matrix = SKMatrix.CreateRotationDegrees(degrees, bounds.MidX, bounds.MidY)
        path.Transform matrix

    let setBrushFill (brush: SKPaint) color =
        brush.IsStroke <- false
        brush.Style <- SKPaintStyle.Fill
        brush.Color <- color

    let setBrushStroke (brush: SKPaint) color width =
        brush.IsStroke <- true
        brush.StrokeWidth <- width
        brush.Style <- SKPaintStyle.Stroke
        brush.Color <- color

    let getPrettyXml (document: XmlDocument) =
        let stringBuilder = new StringBuilder()
        let settings = XmlWriterSettings(Indent = true)

        use writer = XmlWriter.Create(stringBuilder, settings)
        document.Save writer

        stringBuilder.ToString()

    let rec disposeAll (list: IDisposable list) =
        match list with
        | head :: tail -> 
            head.Dispose()
            disposeAll tail
        | [] -> ()

    