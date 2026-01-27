module Drawable exposing (..)

import Camera exposing (Camera, Vec2, worldPosToCamPos)
import Canvas exposing (circle)


drawCircle : Camera -> Vec2 -> Float -> Canvas.Shape
drawCircle cam c r =
    circle (worldPosToCamPos cam c) (cam.zoom * r)
