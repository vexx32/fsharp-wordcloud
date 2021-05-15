namespace wordcloud

open System
open System.Xml
open System.Text
open SkiaSharp
open Constants

module Utils =

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

    type BubbleShape =
    | Rectangle
    | Square
    | Circle
    | Oval

    /// Defines an implicit conversion operator to convert one type to another if an op_Implicit conversion
    /// exists to convert <c>^a</c> to <c>^b</c>.
    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

    let private random = Random()

    let GetBrush color wordSize strokeWidth typeface =
        new SKPaint(
            Typeface = typeface,
            TextSize = wordSize,
            Style = SKPaintStyle.StrokeAndFill,
            Color = color,
            StrokeWidth = wordSize * strokeWidth * StrokeBaseScale,
            IsStroke = false,
            IsAutohinted = true,
            IsAntialias = true)

    let GetPlainBrush = GetBrush SKColors.Black

    let AverageCharArea (typeface: SKTypeface) =
        let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        use brush = GetPlainBrush 1f 0f typeface
        use path = brush.GetTextPath(alphabet, x = 0f, y = 0f)

        let bounds = path.TightBounds
        bounds.Width * bounds.Height / float32 alphabet.Length

    /// <summary>
    /// Gets the angle increment value based on the radius and step values given.
    /// </summary>
    /// <param name="radius">The distance from the centre point.</param>
    /// <param name="step">The radial step increment value.</param>
    /// <returns></returns>
    let GetAngleIncrement (radius: float32) step =
        step * BaseAngularIncrement
        / (15 * int (Math.Sqrt(float radius)))

    let GetEnclosingSquare (rectangle: SKRect) =
        match rectangle with
        | wide when wide.Width > wide.Height -> SKRect.Inflate(wide, x = 0f, y = (wide.Width - wide.Height) / 2f)
        | tall when tall.Height > tall.Width -> SKRect.Inflate(tall, x = (tall.Height - tall.Width) / 2f, y = 0f)
        | square -> square

    let Shuffle (list: 'a list) =
        let mutable index = list.Length
        let array = List.toArray list

        while (index > 1) do
            let otherIndex = random.Next index
            index <- index - 1

            let temp = array.[index]
            array.[index] <- array.[otherIndex]
            array.[otherIndex] <- temp

        List.ofArray array

    let RandomFloat min max =
        if min >= max then
            max
        else
            let range = max - min
            float32 (random.NextDouble()) * range + min
       
    let GetRandomFloats min max =
        let minCount = 3
        let maxCount = 17

        [ for _ in minCount .. maxCount -> RandomFloat min max ]

    let GetRotationAngles orientations =
        match orientations with
        | Some Vertical -> Shuffle [0f; 90f]
        | Some FlippedVertical -> Shuffle [0f; -90f]
        | Some EitherVertical -> Shuffle [0f; 90f; -90f]
        | Some UprightDiagonal -> Shuffle [-90f .. 45f .. 90f]
        | Some InvertedDiagonal -> Shuffle [90f; 135f; -135f; -90f; 180f]
        | Some AnyDiagonal -> Shuffle [45f; 90f; 135f; 180f; -135f; -90f; -45f; 0f]
        | Some AnyUpright -> GetRandomFloats -90f 91f
        | Some AnyInverted -> GetRandomFloats 90f 271f
        | Some Any -> GetRandomFloats 0f 361f
        | None -> [ 0f ]

    let IsMostlyVertical angle =
        let remainder = Math.Abs(angle % 180f)
        135f > remainder && remainder > 45f

    let FallsOutside (region: SKRegion) (rectangle: SKRect) =
        let bounds: SKRect = !> region.Bounds
        rectangle.Top < bounds.Top
        || rectangle.Bottom > bounds.Bottom
        || rectangle.Left < bounds.Left
        || rectangle.Right > bounds.Right
    
    let IsEmpty (region: SKRegion) = region.Bounds.IsEmpty

    let IntersectsRectangle (region: SKRegion) rectangle =
        if IsEmpty region then
            false
        else
            rectangle 
            |> SKRectI.Round 
            |> region.Intersects

    let IntersectsPath (region: SKRegion) (path: SKPath) =
        if IsEmpty region then
            false
        else
            region.Intersects(path)

    let MoveTo (point: SKPoint) (path: SKPath) =
        let midpoint = new SKPoint(path.TightBounds.MidX, path.TightBounds.MidY)
        path.Offset(point - midpoint)

    let Rotate (path: SKPath) degrees =
        let bounds = path.TightBounds
        let matrix = SKMatrix.CreateRotationDegrees(degrees, bounds.MidX, bounds.MidY)
        path.Transform matrix

    let SetFill color (brush: SKPaint) =
        brush.IsStroke <- false
        brush.Style <- SKPaintStyle.Fill
        brush.Color <- color

    let SetBrushStroke color width (brush: SKPaint) =
        brush.IsStroke <- true
        brush.StrokeWidth <- width
        brush.Style <- SKPaintStyle.Stroke
        brush.Color <- color

    let GetPrettyXml (document: XmlDocument) =
        let stringBuilder = new StringBuilder()
        let settings = XmlWriterSettings(Indent = true)

        use writer = XmlWriter.Create(stringBuilder, settings)
        document.Save writer

        stringBuilder.ToString()

    let rec DisposeAll (list: IDisposable list) =
        match list with
        | head :: tail -> 
            head.Dispose()
            DisposeAll tail
        | [] -> ()
