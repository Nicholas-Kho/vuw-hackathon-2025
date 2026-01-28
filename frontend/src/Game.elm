module Game exposing (..)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Camera exposing (mkCamera)
import Canvas
import Canvas.Settings
import Color
import Drawable exposing (drawCircle, renderGrid)
import GameState exposing (..)
import Generated.BackendApi exposing (InitialGameState, getStart)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Http exposing (Error(..))
import PlayerInput
import RemoteData exposing (RemoteData(..))
import String
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
            Html.div []
                [ showGame okm.size okm
                ]


showGame : CanvasSize -> OkModel -> Html Msg
showGame ( w, h ) okm =
    Canvas.toHtml ( w, h )
        [ style "display" "block"
        , style "box-sizing" "border-box"
        , PlayerInput.scrollAttr Input
        , PlayerInput.grabbyCursor okm.input
        ]
        [ Canvas.shapes
            [ Canvas.Settings.fill Color.lightGrey ]
            [ Canvas.rect ( 0, 0 ) (toFloat w) (toFloat h) ]
        , Canvas.shapes [] [ drawCircle okm.game.cam ( 0, 0 ) 50 ]
        , renderGrid okm.game.cam 100
        ]


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
                    ( Good
                        { okm
                            | size = ( w, h )
                            , game = resizeCamera ( toFloat w, toFloat h ) okm.game
                        }
                    , Cmd.none
                    )

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
                , game = GameState.fromInitial (mkCamera ( 500, 500 )) igs
                }
            , Task.perform getVpSize getViewport
            )


tickGame : OkModel -> OkModel
tickGame m =
    let
        ( userInputs, resetInp ) =
            PlayerInput.consume m.input
    in
    { m | input = resetInp, game = handleInputs userInputs m.game }
