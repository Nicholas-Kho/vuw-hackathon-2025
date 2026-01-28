module Camera exposing
    ( Camera
    , Vec2
    , focusOn
    , mkCamera
    , moveCam
    , panTo
    , showCam
    , stopAnimation
    , tickCam
    , worldPosToCamPos
    , zoomAbout
    , zoomCam
    )

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
    | FocusOn FocusOnParams


type alias FocusOnParams =
    { currentPos : Vec2
    , targetPos : Vec2
    , currentZoom : Float
    , targetZoom : Float
    }


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


expInterp : Float -> Float -> Float -> Float -> Float
expInterp k deltaMs current target =
    let
        deltaS =
            deltaMs / 1000

        alpha =
            1 - e ^ (-k * deltaS)

        nextPos =
            current + (target - current) * alpha
    in
    nextPos


expInterpVec : Float -> Float -> Vec2 -> Vec2 -> Vec2
expInterpVec k deltaMs current target =
    let
        deltaS =
            deltaMs / 1000

        alpha =
            1 - e ^ (-k * deltaS)

        nextPos =
            vecFromTo current target |> vScale alpha |> vAdd current
    in
    nextPos


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
vecFromTo a b =
    vMinus b a


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


zoomAbout : Vec2 -> Float -> Camera -> Camera
zoomAbout curPos dz cam =
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


tickAnimation : Float -> Animation -> Maybe Animation
tickAnimation deltaMs anim =
    case anim of
        PanTo params ->
            let
                nextPos =
                    expInterpVec 8 deltaMs params.cur params.target

                areWeThere =
                    vDistSqare params.cur nextPos < 1

                nextParams =
                    { params | cur = nextPos }
            in
            if areWeThere then
                Nothing

            else
                Just (PanTo nextParams)

        FocusOn params ->
            let
                nextPos =
                    expInterpVec 8 deltaMs params.currentPos params.targetPos

                nextZoom =
                    expInterp 2 deltaMs params.currentZoom params.targetZoom

                areWeTherePos =
                    vDistSqare params.currentPos params.targetPos < 1

                areWeThereZoom =
                    abs (params.currentZoom - params.targetZoom) < 0.1

                areWeThere =
                    areWeTherePos && areWeThereZoom

                nextAnim =
                    FocusOn { params | currentPos = nextPos, currentZoom = nextZoom }
            in
            if areWeThere then
                Nothing

            else
                Just nextAnim


applyAnimation : Animation -> Camera -> Camera
applyAnimation anim cam =
    case anim of
        PanTo pms ->
            { cam | worldPos = pms.cur }

        FocusOn pms ->
            -- Assumes that the torget zoom is in an acceptable range.
            { cam | worldPos = pms.currentPos, zoom = pms.currentZoom }


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


panTo : Vec2 -> Camera -> Camera
panTo pos cam =
    { cam | currentAnimation = Just <| PanTo <| { cur = cam.worldPos, target = pos } }


focusOn : Vec2 -> Float -> Camera -> Camera
focusOn pos zoom cam =
    let
        ( minZoom, maxZoom ) =
            cam.zoomRange

        clampedZoom =
            clamp minZoom maxZoom zoom
    in
    { cam
        | currentAnimation =
            Just <|
                FocusOn <|
                    { currentPos = cam.worldPos
                    , currentZoom = cam.zoom
                    , targetPos = pos
                    , targetZoom = clampedZoom
                    }
    }


stopAnimation : Camera -> Camera
stopAnimation cam =
    { cam | currentAnimation = Nothing }
