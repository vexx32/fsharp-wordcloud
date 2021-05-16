namespace wordcloud

open System
open Utils
open Colors
open Words
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
        
    let rec CreateStringList input =
        match input with
        | head :: tail -> string head :: CreateStringList tail
        | [] -> []

    let internal stringEquals a b =
        String.Equals(a, b, StringComparison.CurrentCultureIgnoreCase)

    let internal listOrEmpty list =
        match list with
        | Some l -> l
        | None -> []

    let internal keepWord includeWords excludeWords allowStopWords word =
        let includeList = listOrEmpty includeWords
        let excludeList = listOrEmpty excludeWords

        if includeList.Length > 0 && includeList |> List.contains word then
            true
        elif excludeList.Length > 0 && excludeList |> List.contains word then
            false
        elif not allowStopWords && word |> IsStopWord then
            false
        else
            CountLetters word >= 2

    let GetWords (lines: string list) includeWords excludeWords allowStopWords =
        lines
        |> Seq.map (fun str ->
            str.Split(SplitChars, StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun word ->
                InvalidWordChars.Replace(word, ""))
            |> Seq.filter (fun word ->
                word
                |> keepWord includeWords excludeWords allowStopWords))
        |> Seq.concat
        |> List.ofSeq
