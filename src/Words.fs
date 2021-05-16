namespace wordcloud

open System
open System.Text.RegularExpressions
open SkiaSharp
open Utils
open Extensions
open Images

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

        member this.Bounds =
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

    type WordListStatistics =
        { AverageLength: float32
          AverageFrequency: float32
          TotalFrequency: float32
          TotalLength: int
          Count: int }

    let GetStatistics (list: Word list) =
        let count = list.Length

        let rec accumulateValues frequency length (wordList: Word list) =
            match wordList with
            | head :: tail ->
                tail
                |> accumulateValues (frequency + head.Size) (length + head.Text.Length)
            | [] -> (frequency, length)

        let (totalFrequency, totalLength) = list |> accumulateValues 0f 0
        let averageFrequency = totalFrequency / float32 count
        let averageLength = float32 totalLength / float32 count

        { AverageLength = averageLength
          AverageFrequency = averageFrequency
          TotalFrequency = totalFrequency
          TotalLength = totalLength
          Count = count }

    let GetScaledSize (word: Word) scale = word.Size * scale

    let RotateWord degrees (word: Word) =
        match word.Path with
        | Some path ->
            word.Angle <- word.Angle + degrees
            path |> Rotate <| degrees

            match word.Bubble with
            | Some path -> path |> Rotate <| degrees
            | None -> ()
        | None -> ()

    let MoveWordTo point (word: Word) =
        match word.Path with
        | Some path ->
            path |> MoveTo point

            match word.Bubble with
            | Some bubble -> bubble |> MoveTo point
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

        let radius =
            Math.Max(rectangle.Width, rectangle.Height) / 16f

        use bubble =
            new SKRoundRect(rectangle |> GetEnclosingSquare, radius)

        path.AddRoundRect bubble

        path

    let private getCirclePath (rectangle: SKRect) =
        let path = new SKPath()

        let radius =
            Math.Max(rectangle.Width, rectangle.Height) / 2f

        path.AddCircle(x = rectangle.MidX, y = rectangle.MidY, radius = radius)

        path

    let private getOvalPath (rectangle: SKRect) =
        let padding =
            1f / 10f
            * (Math.Sqrt
               <| float (rectangle.Width * rectangle.Height)
               |> float32)

        let path = new SKPath()

        SKRect.Inflate(rectangle, x = padding, y = padding)
        |> path.AddOval

        path

    let GetWordBubble shape (word: Word) =
        match word.Path with
        | Some path ->
            let bounds =
                SKRect.Inflate(word.Bounds, word.Padding, word.Padding)

            match shape with
            | Some Rectangle -> getRectanglePath bounds |> Some
            | Some Square -> getSquarePath bounds |> Some
            | Some Circle -> getCirclePath bounds |> Some
            | Some Oval -> getOvalPath bounds |> Some
            | None -> None
        | None -> None

    let private rectFitsIn (image: Image) padding rectangle =
        let paddedRect =
            SKRect.Inflate(rectangle, x = padding, y = padding)

        not (IntersectsRectangle image.OccupiedSpace rectangle)

    let private getPaddedPath padding (path: SKPath) =
        let scale =
            SKMatrix.CreateScale(
                x = 1f + padding / path.TightBounds.Width,
                y = 1f + padding / path.TightBounds.Height,
                pivotX = path.TightBounds.MidX,
                pivotY = path.TightBounds.MidY
            )

        let resultPath = new SKPath()
        path.Transform(scale, resultPath)

        resultPath

    let private pathFitsIn (image: Image) padding (path: SKPath) =
        use paddedPath = path |> getPaddedPath padding
        not (IntersectsPath image.OccupiedSpace paddedPath)

    let WordFitsIn (image: Image) (word: Word) =
        if word.Bounds |> FallsOutside image.ClippingRegion then
            false
        else
            match word.Bubble with
            | Some bubble -> bubble |> pathFitsIn image word.Padding
            | None -> word.Bounds |> rectFitsIn image word.Padding

    let InvalidWordChars = Regex(@"^[^a-zA-Z0-9]+|[^a-zA-Z0-9]+$")

    let internal nonAlphaChars = Regex("[^a-zA-Z-]")

    let CountLetters word =
        nonAlphaChars.Replace(word, "").Length

    let SplitChars =
        [|
          ' '
          '\n'
          '\t'
          '\r'
          '.'
          ','
          ';'
          ':'
          '?'
          '!'
          '\\'
          '/'
          '|'
          '"'
          '“'
          '”'
          '{'
          '}'
          '['
          ']'
          '('
          ')'
          '<'
          '>'
          '*'
          '#'
          '%'
          '^'
          '&'
          '+'
          '=' |]

    let private stopWords =
        [ "a"
          "about"
          "above"
          "after"
          "again"
          "against"
          "all"
          "am"
          "an"
          "and"
          "any"
          "are"
          "aren't"
          "as"
          "at"
          "be"
          "because"
          "been"
          "before"
          "being"
          "below"
          "between"
          "both"
          "but"
          "by"
          "can't"
          "cannot"
          "could"
          "couldn't"
          "did"
          "didn't"
          "do"
          "does"
          "doesn't"
          "doing"
          "don't"
          "down"
          "during"
          "each"
          "few"
          "for"
          "from"
          "further"
          "had"
          "hadn't"
          "has"
          "hasn't"
          "have"
          "haven't"
          "having"
          "he"
          "he'd"
          "he'll"
          "he's"
          "her"
          "here"
          "here's"
          "hers"
          "herself"
          "him"
          "himself"
          "his"
          "how"
          "how's"
          "i"
          "i'd"
          "i'll"
          "i'm"
          "i've"
          "if"
          "in"
          "into"
          "is"
          "isn't"
          "it"
          "it's"
          "its"
          "itself"
          "let's"
          "me"
          "more"
          "most"
          "mustn't"
          "my"
          "myself"
          "no"
          "nor"
          "not"
          "of"
          "off"
          "on"
          "once"
          "only"
          "or"
          "other"
          "ought"
          "our"
          "ours"
          "ourselves"
          "out"
          "over"
          "own"
          "same"
          "shan't"
          "she"
          "she'd"
          "she'll"
          "she's"
          "should"
          "shouldn't"
          "so"
          "some"
          "such"
          "than"
          "that"
          "that's"
          "the"
          "their"
          "theirs"
          "them"
          "themselves"
          "then"
          "there"
          "there's"
          "these"
          "they"
          "they'd"
          "they'll"
          "they're"
          "they've"
          "this"
          "those"
          "through"
          "to"
          "too"
          "under"
          "until"
          "up"
          "very"
          "was"
          "wasn't"
          "we"
          "we'd"
          "we'll"
          "we're"
          "we've"
          "were"
          "weren't"
          "what"
          "what's"
          "when"
          "when's"
          "where"
          "where's"
          "which"
          "while"
          "who"
          "who's"
          "whom"
          "why"
          "why's"
          "with"
          "won't"
          "would"
          "wouldn't"
          "you"
          "you'd"
          "you'll"
          "you're"
          "you've"
          "your"
          "yours"
          "yourself"
          "yourselves" ]

    let IsStopWord word = stopWords |> List.contains word
