module PlayerInput exposing (..)

import Browser.Events as Events
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Decode
import Tuple exposing (first, second)


type InputAction
    = Center


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


type MouseState
    = Dragging
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

        DidAction a ->
            { model | bufferedActions = a :: model.bufferedActions }


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

        Action _ ->
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
