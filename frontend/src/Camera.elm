module Camera exposing (..)

import Html exposing (Html)
import String exposing (fromFloat)


type alias Camera =
    { worldPos : Vec2
    , zoom : Float
    , zoomRange : ( Float, Float )
    , canvasSize : Vec2
    , currentAnimation : Maybe Animation
    }


type Animation
    = PanTo PanParams


type alias PanParams =
    { cur : Vec2
    , target : Vec2
    }


type alias Vec2 =
    ( Float, Float )


mkCamera : Vec2 -> Camera
mkCamera csize =
    { worldPos = ( 0, 0 )
    , zoom = 1
    , zoomRange = ( 0.1, 2 )
    , canvasSize = csize
    , currentAnimation = Nothing
    }


setPos : ( Float, Float ) -> Camera -> Camera
setPos p cam =
    { cam | worldPos = p }


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


vAdd : Vec2 -> Vec2 -> Vec2
vAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


vMinus : Vec2 -> Vec2 -> Vec2
vMinus ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


vScale : Float -> Vec2 -> Vec2
vScale a ( x, y ) =
    ( a * x, a * y )


vDistSqare : Vec2 -> Vec2 -> Float
vDistSqare v1 v2 =
    let
        ( dx, dy ) =
            vMinus v1 v2
    in
    (dx * dx) + (dy * dy)


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


setAnimation : Animation -> Camera -> Camera
setAnimation anim cam =
    { cam | currentAnimation = Just anim }


tickAnimation : Float -> Animation -> Maybe Animation
tickAnimation deltaMs anim =
    case anim of
        PanTo pms ->
            let
                k =
                    8

                deltaS =
                    deltaMs / 1000

                alpha =
                    1 - e ^ (-k * deltaS)

                nextPos =
                    vAdd pms.cur <| vScale alpha (vMinus pms.target pms.cur)

                areWeThere =
                    vDistSqare pms.cur pms.target < 1

                nextParams =
                    { pms | cur = nextPos }
            in
            if areWeThere then
                Nothing

            else
                Just (PanTo nextParams)


applyAnimation : Animation -> Camera -> Camera
applyAnimation anim cam =
    case anim of
        PanTo pms ->
            { cam | worldPos = pms.cur }


tickCam : Float -> Camera -> Camera
tickCam deltaMs cam =
    case cam.currentAnimation of
        Nothing ->
            cam

        Just anim ->
            let
                nextAnim =
                    tickAnimation deltaMs anim

                newCam =
                    applyAnimation anim cam
            in
            { newCam | currentAnimation = nextAnim }
