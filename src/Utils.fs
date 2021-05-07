namespace wordcloud

open System
open System.Collections.Generic
open SkiaSharp
open Constants
open Extensions

module Utils =

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

        new SKColor(red, green, blue, alpha)
        

