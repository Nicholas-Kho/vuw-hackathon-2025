module Camera exposing (..)

import Html exposing (Html)
import String exposing (fromFloat)


type alias Camera =
    { worldPos : Vec2
    , zoom : Float
    , zoomRange : ( Float, Float )
    }


type alias Vec2 =
    ( Float, Float )


mkCamera : Camera
mkCamera =
    { worldPos = ( 0, 0 )
    , zoom = 1
    , zoomRange = ( 0.1, 2 )
    }


moveCam : ( Float, Float ) -> Camera -> Camera
moveCam ( dx, dy ) cam =
    let
        ( x, y ) =
            cam.worldPos
    in
    { cam | worldPos = ( x + dx, y + dy ) }


zoomCam : Float -> Camera -> Camera
zoomCam by cam =
    let
        ( minZoom, maxZoom ) =
            cam.zoomRange

        newZoom =
            clamp minZoom maxZoom (cam.zoom + by)
    in
    { cam | zoom = newZoom }


worldPosToCamPos : Camera -> Vec2 -> Vec2
worldPosToCamPos cam ( x, y ) =
    let
        ( a, b ) =
            ( cam.zoom, 0 )

        ( c, d ) =
            ( 0, cam.zoom )

        ( px, py ) =
            cam.worldPos

        ( dx, dy ) =
            ( x - px, y - py )
    in
    ( a * dx + b * dy
    , c * dx + d * dy
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
