namespace wordcloud

open Utils
open Colors
open SkiaSharp

module WordCloud =
    let PrepareColorSet colorList isMonochrome maxColors =
        let limit = 
            match maxColors with
            | Some x -> x
            | None -> 100

        if isMonochrome then
            colorList
            |> List.map (fun c -> ToGreyscale c)
        else
            colorList
        |> Shuffle
        |> List.take limit
        
