namespace wordcloud
open System

module Extensions =
    type Int32 with
        /// <summary>
        /// Converts the given angular value to radians, assuming the input is the value in degrees.
        /// </summary>
        /// <returns>This value converted to radians.</returns>
        member this.ToRadians =
            float this * Math.PI * 180.0