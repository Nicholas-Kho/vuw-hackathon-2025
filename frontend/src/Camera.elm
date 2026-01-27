module Camera exposing (..)


type alias Camera =
    { worldPos : Vec2
    , basis : Mat2
    }


type alias Vec2 =
    ( Float, Float )


type alias Mat2 =
    { row1 : Vec2
    , row2 : Vec2
    }


iTwo : Mat2
iTwo =
    { row1 = ( 1, 0 )
    , row2 = ( 0, 1 )
    }


mkCamera : Camera
mkCamera =
    { worldPos = ( 0, 0 )
    , basis = iTwo
    }


worldPosToCamPos : Camera -> Vec2 -> Vec2
worldPosToCamPos cam ( x, y ) =
    let
        ( a, b ) =
            cam.basis.row1

        ( c, d ) =
            cam.basis.row2

        ( px, py ) =
            cam.worldPos

        ( dx, dy ) =
            ( x - px, y - py )
    in
    ( a * dx + b * dy
    , c * dx + d * dy
    )
