module Drawable exposing (..)

import Camera exposing (Camera, Vec2, worldPosToCamPos)
import Canvas exposing (circle)


drawCircle : Camera -> Vec2 -> Float -> Canvas.Shape
drawCircle cam c r =
    circle (worldPosToCamPos cam c) (cam.zoom * r)


snapDown : Float -> Float -> Float
snapDown val step =
    step * toFloat (floor (val / step))


type alias Line =
    { startX : Float
    , startY : Float
    , endX : Float
    , endY : Float
    }


mkLine : ( Float, Float ) -> ( Float, Float ) -> Line
mkLine ( sx, sy ) ( ex, ey ) =
    { startX = sx
    , startY = sy
    , endX = ex
    , endY = ey
    }


worldLineToCamLine : Camera -> Line -> Line
worldLineToCamLine c l =
    let
        s =
            worldPosToCamPos c ( l.startX, l.startY )

        e =
            worldPosToCamPos c ( l.endX, l.endY )
    in
    mkLine s e


drawLines : List Line -> Canvas.Shape
drawLines lines =
    case lines of
        [] ->
            circle ( 0, 0 ) 0

        start :: ls ->
            Canvas.path ( start.startX, start.startY ) <|
                Canvas.lineTo ( start.endX, start.endY )
                    :: List.concatMap
                        (\l ->
                            [ Canvas.moveTo ( l.startX, l.startY )
                            , Canvas.lineTo ( l.endX, l.endY )
                            ]
                        )
                        ls


drawGrid : Camera -> Float -> Canvas.Shape
drawGrid cam squareSize =
    let
        ( px, py ) =
            cam.worldPos

        ( w, h ) =
            cam.canvasSize

        halfWorldW =
            w / (2 * cam.zoom)

        halfWorldH =
            h / (2 * cam.zoom)

        worldLeft =
            px - halfWorldW

        worldRight =
            px + halfWorldW

        worldTop =
            py - halfWorldH

        worldBottom =
            py + halfWorldH

        startX =
            snapDown worldLeft squareSize

        endX =
            worldRight

        startY =
            snapDown worldTop squareSize

        endY =
            worldBottom

        xs =
            List.range 0 (ceiling ((endX - startX) / squareSize))
                |> List.map (\i -> startX + toFloat i * squareSize)

        ys =
            List.range 0 (ceiling ((endY - startY) / squareSize))
                |> List.map (\i -> startY + toFloat i * squareSize)

        linesV =
            List.map (\x -> mkLine ( x, worldTop ) ( x, worldBottom )) xs

        linesH =
            List.map (\y -> mkLine ( worldLeft, y ) ( worldRight, y )) ys
    in
    drawLines <| List.map (worldLineToCamLine cam) <| linesV ++ linesH
