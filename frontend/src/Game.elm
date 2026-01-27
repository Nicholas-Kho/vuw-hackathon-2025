module Game exposing (..)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Canvas
import GameState exposing (..)
import Generated.BackendApi exposing (InitialGameState, getStart)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Http exposing (Error(..))
import PlayerInput
import RemoteData exposing (RemoteData(..))
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subs
        }


subs : Model -> Sub Msg
subs m =
    Sub.batch
        [ onResize Resize
        , onAnimationFrameDelta (\_ -> Tick)
        , getInputSubs m
        ]


getInputSubs : Model -> Sub Msg
getInputSubs m =
    case m of
        Good okm ->
            Sub.map Input <| PlayerInput.subscriptions okm.input

        _ ->
            Sub.none


type alias CanvasSize =
    ( Int, Int )


type Model
    = Initialising
    | UnrecoverableFail String
    | Good OkModel


type alias OkModel =
    { size : CanvasSize
    , input : PlayerInput.Model
    , game : GameState
    }


type Msg
    = StartResponse (Result Http.Error InitialGameState)
    | Resize Int Int
    | Input PlayerInput.Msg
    | Tick


init : ( Model, Cmd Msg )
init =
    ( Initialising
    , getStart StartResponse
    )


getVpSize : Browser.Dom.Viewport -> Msg
getVpSize vp =
    Resize (floor vp.viewport.width) (floor vp.viewport.height)


view : Model -> Html Msg
view model =
    case model of
        Initialising ->
            text "Loading, please wait..."

        UnrecoverableFail why ->
            text <| "There was an error I can't recover from: " ++ why

        Good okm ->
            showGame okm.size okm.game


showGame : CanvasSize -> GameState -> Html Msg
showGame ( w, h ) gs =
    Canvas.toHtml ( w, h )
        [ style "border" "10px solid rgba(0,0,0,0.1)"
        , style "display" "block"
        , style "width" (String.fromInt w ++ "px")
        , style "height" (String.fromInt h ++ "px")
        , style "box-sizing" "border-box"
        , PlayerInput.scrollAttr Input
        ]
        []


showErr : Error -> String
showErr err =
    case err of
        NetworkError ->
            "Network error"

        Timeout ->
            "timeout"

        BadUrl s ->
            s

        BadStatus s ->
            String.fromInt s

        BadBody b ->
            b


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        UnrecoverableFail _ ->
            ( model, Cmd.none )

        Good okm ->
            case msg of
                Resize w h ->
                    ( Good { okm | size = ( w, h ) }, Cmd.none )

                Input i ->
                    ( Good { okm | input = PlayerInput.update i okm.input }, Cmd.none )

                Tick ->
                    ( Good (tickGame okm), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Initialising ->
            case msg of
                StartResponse wd ->
                    handleStartResponse wd

                _ ->
                    ( Initialising, Cmd.none )


handleStartResponse : Result Http.Error InitialGameState -> ( Model, Cmd Msg )
handleStartResponse res =
    case res of
        Result.Err httpe ->
            ( UnrecoverableFail <| showErr httpe, Cmd.none )

        Result.Ok igs ->
            ( Good
                { size = ( 500, 500 )
                , input = PlayerInput.init
                , game = GameState.fromInitial igs
                }
            , Task.perform getVpSize getViewport
            )


tickGame : OkModel -> OkModel
tickGame m =
    let
        ( userInputs, resetInp ) =
            PlayerInput.consume m.input
    in
    { m | input = resetInp }
