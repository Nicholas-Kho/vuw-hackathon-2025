module PlayerInput exposing (..)

import Browser.Events as Events
import Html exposing (Attribute, Html, text)
import Html.Events
import Json.Decode as Decode
import Tuple exposing (first, second)


type UserInput
    = Pan Float Float
    | Zoom Float


type Msg
    = MouseDown
    | MouseUp
    | MouseMove Float Float
    | MouseScroll Float


type MouseState
    = Dragging
    | Idle


type alias Model =
    { mouse : MouseState
    , scrolled : Float
    , mouseDelta : ( Float, Float )
    }


init : Model
init =
    { mouse = Idle
    , scrolled = 0
    , mouseDelta = ( 0, 0 )
    }


scrollAttr : (Msg -> parentMsg) -> Attribute parentMsg
scrollAttr liftMsg =
    Html.Events.on "wheel" (Decode.map liftMsg mouseScrollDecoder)


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.mouse of
        Idle ->
            Events.onMouseDown (Decode.succeed MouseDown)

        Dragging ->
            Sub.batch
                [ Events.onMouseUp (Decode.succeed MouseUp)
                , Events.onMouseMove mouseMoveDecoder
                ]


mouseMoveDecoder : Decode.Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


mouseScrollDecoder : Decode.Decoder Msg
mouseScrollDecoder =
    Decode.map MouseScroll (Decode.field "deltaY" Decode.float)


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown ->
            { model | mouse = Dragging }

        MouseUp ->
            { model | mouse = Idle }

        MouseMove x y ->
            { model | mouseDelta = ( x, y ) }

        MouseScroll s ->
            { model | scrolled = model.scrolled + s }


addMove : Model -> Float -> Float -> Model
addMove m x y =
    let
        ( ax, ay ) =
            m.mouseDelta
    in
    { m | mouseDelta = ( ax + x, ay + y ) }


mkInputs : Model -> List UserInput
mkInputs model =
    List.filter goodInput
        [ Pan (first model.mouseDelta) (second model.mouseDelta)
        , Zoom model.scrolled
        ]


consume : Model -> ( List UserInput, Model )
consume m =
    ( mkInputs m, { init | mouse = m.mouse } )


isBig : Float -> Bool
isBig f =
    abs f > 0


goodInput : UserInput -> Bool
goodInput uinp =
    case uinp of
        Pan x y ->
            isBig x || isBig y

        Zoom z ->
            isBig z


showInput : Model -> Html msg
showInput i =
    Html.div []
        [ Html.h2 [] [ text "Accumulated input:" ]
        , Html.p [] [ text <| "pan: " ++ showPoint i.mouseDelta ]
        , Html.p [] [ text <| "scroll: " ++ String.fromFloat i.scrolled ]
        ]


showPoint : ( Float, Float ) -> String
showPoint ( x, y ) =
    "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"
