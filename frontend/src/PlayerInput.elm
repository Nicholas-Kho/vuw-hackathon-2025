module PlayerInput exposing (..)

import Browser.Events as Events
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Decode
import Tuple exposing (first, second)


type UserInput
    = Pan Float Float
    | Zoom Float ( Float, Float )


type Msg
    = MouseDown
    | MouseUp
    | MouseMoveRelative Float Float
    | MouseMove Float Float
    | MouseScroll Float


type MouseState
    = Dragging
    | Idle


type alias Model =
    { mouse : MouseState
    , scrolled : Float
    , mouseDelta : ( Float, Float )
    , cursorPos : ( Float, Float )
    }


init : Model
init =
    { mouse = Idle
    , scrolled = 0
    , mouseDelta = ( 0, 0 )
    , cursorPos = ( 0, 0 )
    }


scrollAttr : (Msg -> parentMsg) -> Attribute parentMsg
scrollAttr liftMsg =
    Html.Events.on "wheel" (Decode.map liftMsg mouseScrollDecoder)


grabbyCursor : Model -> Attribute msg
grabbyCursor m =
    case m.mouse of
        Dragging ->
            style "cursor" "grabbing"

        Idle ->
            style "cursor" "grab"


subscriptions : Model -> Sub Msg
subscriptions m =
    let
        rest =
            case m.mouse of
                Idle ->
                    Events.onMouseDown (Decode.succeed MouseDown)

                Dragging ->
                    Sub.batch
                        [ Events.onMouseUp (Decode.succeed MouseUp)
                        , Events.onMouseMove mouseMoveRelativeDecoder
                        ]
    in
    Sub.batch [ rest, Events.onMouseMove mouseMoveDecoder ]


mouseMoveRelativeDecoder : Decode.Decoder Msg
mouseMoveRelativeDecoder =
    Decode.map2 MouseMoveRelative
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


mouseMoveDecoder : Decode.Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


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

        MouseMoveRelative x y ->
            { model | mouseDelta = ( x, y ) }

        MouseMove x y ->
            { model | cursorPos = ( x, y ) }

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
        [ Pan -(first model.mouseDelta) -(second model.mouseDelta)
        , Zoom model.scrolled model.cursorPos
        ]


consume : Model -> ( List UserInput, Model )
consume m =
    ( mkInputs m, { m | scrolled = 0, mouseDelta = ( 0, 0 ) } )


isBig : Float -> Bool
isBig f =
    abs f > 0


goodInput : UserInput -> Bool
goodInput uinp =
    case uinp of
        Pan x y ->
            isBig x || isBig y

        Zoom z _ ->
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
