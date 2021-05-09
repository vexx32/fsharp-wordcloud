namespace wordcloud
open System
open System.Numerics
open SkiaSharp
open Utils
open Extensions

module Shapes =
    [<Struct>]
    type Ellipse = { SemiMinorAxis : float32; AspectRatio : float32; Centre : SKPoint }

    let private random = Random()

    let private getPointOnEllipse ellipse (angle : int) =
        let complexPoint = Complex.FromPolarCoordinates(float ellipse.SemiMinorAxis, angle.ToRadians)
        let x = ellipse.Centre.X + float32 complexPoint.Real * ellipse.AspectRatio
        let y = ellipse.Centre.Y + float32 complexPoint.Imaginary

        new SKPoint(x, y)

    /// <summary>
    /// Returns a <c>list</c> of <see cref="SKPoint"/> values representing the 
    /// </summary>
    /// <param name="ellipse">The ellipse for which to get perimeter points.</param>
    /// <param name="radialStep">The step radial distance between each point.</param>
    /// <returns>A list of points that comprise the ellipse. If the primary axis of the ellipse is <c>0</c> then the only point in the list will be its centre.</returns>
    let GetEllipsePoints radialStep ellipse =
        if (ellipse.SemiMinorAxis = 0f) then
            [ ellipse.Centre ]
        else 
            let startingAngle = random.Next(0, 5) * 90
            let clockwise = random.NextDouble() > 0.5
            
            let increment = 
                if clockwise then
                    GetAngleIncrement ellipse.SemiMinorAxis radialStep
                else
                    - (GetAngleIncrement ellipse.SemiMinorAxis radialStep)

            let maxAngle = 
                if clockwise then
                    startingAngle + 360
                else 
                    startingAngle - 360
            
            [ for angle in startingAngle .. increment .. maxAngle -> getPointOnEllipse ellipse angle ]
