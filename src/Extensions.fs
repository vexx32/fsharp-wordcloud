namespace wordcloud
open System
open SkiaSharp
open Utils

module Extensions =
    type Int32 with
        /// <summary>
        /// Converts the given angular value to radians, assuming the input is the value in degrees.
        /// </summary>
        /// <returns>This value converted to radians.</returns>
        member this.ToRadians =
            float this * Math.PI * 180.0

    type SKRegion with
        member this.SetPath(path: SKPath, usePathBounds: bool) =
            if usePathBounds then
                match path.GetBounds() with
                | true, bounds -> 
                    use clip = new SKRegion()
                    SKRectI.Ceiling bounds 
                    |> clip.SetRect
                    |> ignore

                    this.SetPath(path, clip) |> ignore
                | _ ->
                    this.SetPath path |> ignore

        member this.CombineWithPath (path: SKPath) (operation: SKRegionOperation) =
            use pathRegion = new SKRegion()
            pathRegion.SetPath(path, usePathBounds = true)

            this.Op(pathRegion, operation) |> ignore

        member this.Contains (point: SKPoint) =
            let bounds: SKRect = !> this.Bounds

            bounds.Left < point.X && point.X < bounds.Right
                && bounds.Top < point.Y && point.Y < bounds.Bottom

    type SKPoint with
        member this.Multiply factor =
            new SKPoint(this.X * factor, this.Y * factor)
        member this.IsOutside (region: SKRegion) =
            not (region.Contains this)