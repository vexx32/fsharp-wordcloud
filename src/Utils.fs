namespace wordcloud
open System
open Constants
open Extensions

module Utils =
    
    /// <summary>
    /// Gets the angle increment value based on the radius and step values given.
    /// </summary>
    /// <param name="radius">The distance from the centre point.</param>
    /// <param name="step">The radial step increment value.</param>
    /// <returns></returns>
    let getAngleIncrement (radius : float32) step =
        step * baseAngularIncrement / (15 * int (Math.Sqrt(float radius)))