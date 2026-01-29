module PlayerInput exposing (..)

import Browser.Events as Events
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Decode
import Task
import Time
import Tuple exposing (first, second)


type InputAction
    = Center
    | Click ( Float, Float )


type UserInput
    = Pan Float Float
    | Zoom Float ( Float, Float )
    | Action InputAction


type Msg
    = MouseDown
    | MouseUp
    | MouseMoveRelative Float Float
    | MouseMove Float Float
    | MouseScroll Float
    | DidAction InputAction
    | GotTime Time.Posix


type MouseState
    = Dragging ( Float, Float ) Time.Posix
    | Idle


type alias Model =
    { mouse : MouseState
    , scrolled : Float
    , mouseDelta : ( Float, Float )
    , cursorPos : ( Float, Float )
    , bufferedActions : List InputAction
    }


init : Model
init =
    { mouse = Idle
    , scrolled = 0
    , mouseDelta = ( 0, 0 )
    , cursorPos = ( 0, 0 )
    , bufferedActions = []
    }


scrollAttr : (Msg -> parentMsg) -> Attribute parentMsg
scrollAttr liftMsg =
    Html.Events.on "wheel" (Decode.map liftMsg mouseScrollDecoder)


grabbyCursor : Model -> Attribute msg
grabbyCursor m =
    case m.mouse of
        Dragging _ _ ->
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

                Dragging _ _ ->
                    Sub.batch
                        [ Events.onMouseUp (Decode.succeed MouseUp)
                        , Events.onMouseMove mouseMoveRelativeDecoder
                        ]
    in
    Sub.batch
        [ rest
        , Events.onMouseMove mouseMoveDecoder
        , Events.onKeyDown keyPressDecoder
        ]


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


keyPressDecoder : Decode.Decoder Msg
keyPressDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\k ->
                case k of
                    " " ->
                        Decode.succeed (DidAction Center)

                    _ ->
                        Decode.fail "this key doesn't correspond to an action."
            )


simple : Model -> ( Model, Cmd msg )
simple m =
    ( m, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown ->
            ( model, Task.perform GotTime Time.now )

        GotTime t ->
            simple <| updateClickTime t model

        MouseUp ->
            ( model, Task.perform GotTime Time.now )

        MouseMoveRelative x y ->
            simple { model | mouseDelta = ( x, y ) }

        MouseMove x y ->
            simple { model | cursorPos = ( x, y ) }

        MouseScroll s ->
            simple { model | scrolled = model.scrolled + s }

        DidAction a ->
            simple { model | bufferedActions = a :: model.bufferedActions }


timeDifferenceMs : Time.Posix -> Time.Posix -> Int
timeDifferenceMs a b =
    let
        ams =
            Time.posixToMillis a

        bms =
            Time.posixToMillis b
    in
    ams - bms


wasAClick : Int -> ( Float, Float ) -> ( Float, Float ) -> Bool
wasAClick clickDurationMs startPos endPos =
    let
        ( sx, sy ) =
            startPos

        ( ex, ey ) =
            endPos

        ( dx, dy ) =
            ( ex - sx, ey - sy )

        distanceMovedSquare =
            (dx * dx) + (dy * dy)
    in
    distanceMovedSquare < 16 && clickDurationMs < 135


updateClickTime : Time.Posix -> Model -> Model
updateClickTime t model =
    case model.mouse of
        Idle ->
            { model | mouse = Dragging model.cursorPos t }

        Dragging startPos startedAtTime ->
            if wasAClick (timeDifferenceMs t startedAtTime) startPos model.cursorPos then
                { model
                    | bufferedActions = Click startPos :: model.bufferedActions
                    , mouse = Idle
                }

            else
                { model | mouse = Idle }


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
        ++ List.map Action model.bufferedActions


consume : Model -> ( List UserInput, Model )
consume m =
    ( mkInputs m
    , { m
        | scrolled = 0
        , mouseDelta = ( 0, 0 )
        , bufferedActions = []
      }
    )


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

        _ ->
            True


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
