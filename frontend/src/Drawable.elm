module Drawable exposing (..)

import BackendWrapper exposing (getContent)
import Camera exposing (Camera, Vec2, worldPosToCamPos)
import Canvas exposing (circle)
import Canvas.Settings exposing (stroke)
import Color exposing (rgba)
import Generated.BackendApi exposing (NodeContent)
import Navigation exposing (NavTree, getTree)
import Tree exposing (Tree, mkCartesian, toPolarEdges, toPolarNodes)


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


renderGrid : Camera -> Float -> Canvas.Renderable
renderGrid cam gridSize =
    let
        minZoom =
            Tuple.first cam.zoomRange

        gridAlphaMinor =
            clamp 0.01 1 (sqrt cam.zoom - sqrt minZoom)

        gridAlphaMajor =
            clamp 0.1 1 (sqrt cam.zoom - sqrt minZoom)

        gridColor =
            rgba 0 0 0
    in
    Canvas.group []
        [ Canvas.shapes [ stroke <| gridColor gridAlphaMinor ] [ drawGrid cam gridSize ]
        , Canvas.shapes [ stroke <| gridColor gridAlphaMajor ] [ drawGrid cam (gridSize * 5) ]
        ]


treeEdges : Camera -> Tree a -> Canvas.Shape
treeEdges cam t =
    let
        pToScreen =
            mkCartesian >> .pos >> Camera.worldPosToCamPos cam

        eToLine e =
            let
                start =
                    pToScreen e.from

                end =
                    pToScreen e.to
            in
            mkLine start end
    in
    t
        |> toPolarEdges
        |> List.map eToLine
        |> drawLines


drawNode : Camera -> Vec2 -> NodeContent -> Canvas.Renderable
drawNode cam pos _ =
    Canvas.shapes [ Canvas.Settings.fill Color.red ] [ drawCircle cam pos 20 ]


drawNavTree : Camera -> NavTree -> Canvas.Renderable
drawNavTree cam t =
    let
        tree =
            Tree.map (Tuple.second >> getContent) <| getTree t

        nodes =
            toPolarNodes tree
                |> List.map (\p -> drawNode cam (mkCartesian p |> .pos) p.content)

        edges =
            Canvas.shapes [] [ treeEdges cam tree ]
    in
    Canvas.group [] (edges :: nodes)
