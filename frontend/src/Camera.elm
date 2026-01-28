module Camera exposing (..)

import Html exposing (Html)
import String exposing (fromFloat)


type alias Camera =
    { worldPos : Vec2
    , zoom : Float
    , zoomRange : ( Float, Float )
    , canvasSize : Vec2
    }


type alias Vec2 =
    ( Float, Float )


mkCamera : Vec2 -> Camera
mkCamera csize =
    { worldPos = ( 0, 0 )
    , zoom = 1
    , zoomRange = ( 0.1, 2 )
    , canvasSize = csize
    }


moveCam : ( Float, Float ) -> Camera -> Camera
moveCam ( dx, dy ) cam =
    let
        ( x, y ) =
            cam.worldPos
    in
    { cam | worldPos = ( x + dx, y + dy ) }


exp : Float -> Float
exp x =
    e ^ x


zoomCam : Float -> Camera -> Camera
zoomCam dz cam =
    let
        ( minZoom, maxZoom ) =
            cam.zoomRange

        sensitivity =
            0.002

        newZoom =
            cam.zoom * exp (-dz * sensitivity)
    in
    { cam | zoom = clamp minZoom maxZoom newZoom }


vecFromTo : Vec2 -> Vec2 -> Vec2
vecFromTo ( xa, ya ) ( xb, yb ) =
    ( xb - xa, yb - ya )


zoomAbout : Camera -> Vec2 -> Float -> Camera
zoomAbout cam curPos dz =
    let
        cursorToWorldBefore =
            camPosToWorldPos cam curPos

        zoomedCam =
            zoomCam dz cam

        cursorToWorldAfter =
            camPosToWorldPos zoomedCam curPos
    in
    moveCam (vecFromTo cursorToWorldAfter cursorToWorldBefore) zoomedCam


worldPosToCamPos : Camera -> Vec2 -> Vec2
worldPosToCamPos cam ( x, y ) =
    let
        ( a, b ) =
            ( cam.zoom, 0 )

        ( c, d ) =
            ( 0, cam.zoom )

        ( px, py ) =
            cam.worldPos

        ( cx, cy ) =
            ( Tuple.first cam.canvasSize / 2, Tuple.second cam.canvasSize / 2 )

        ( dx, dy ) =
            ( x - px, y - py )
    in
    ( a * dx + b * dy + cx
    , c * dx + d * dy + cy
    )


camPosToWorldPos : Camera -> Vec2 -> Vec2
camPosToWorldPos cam ( x, y ) =
    let
        ( px, py ) =
            cam.worldPos

        ( cx, cy ) =
            ( Tuple.first cam.canvasSize / 2, Tuple.second cam.canvasSize / 2 )
    in
    ( (x - cx) / cam.zoom + px
    , (y - cy) / cam.zoom + py
    )


showCam : Camera -> Html msg
showCam cam =
    Html.div []
        [ Html.h2 []
            [ Html.text <|
                "Cam pos: ("
                    ++ (fromFloat <| Tuple.first cam.worldPos)
                    ++ ", "
                    ++ (fromFloat <| Tuple.second cam.worldPos)
                    ++ ")"
            ]
        , Html.h2 []
            [ Html.text <| "Cam zoom: " ++ fromFloat cam.zoom ]
        ]
