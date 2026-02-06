module Game exposing (..)

import BackendWrapper exposing (getNode, makeExpandParams, xformSubgraph)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Camera exposing (mkCamera)
import Canvas
import Canvas.Settings
import Canvas.Settings.Advanced exposing (GlobalCompositeOperationMode(..))
import Color
import Drawable exposing (renderGrid)
import Element exposing (Element)
import FinishRoamButton exposing (finishRoamButton)
import GameEndScreen exposing (endScreen)
import GameState exposing (..)
import Generated.BackendApi exposing (InitialGameState, Subgraph, getStart, postExpand)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Http exposing (Error(..))
import PlayerInput
import RemoteData exposing (RemoteData(..))
import SidePanel exposing (sidePanel)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> subs
        }


subs : Sub Msg
subs =
    Sub.batch
        [ onResize Resize
        , onAnimationFrameDelta Tick
        , getInputSubs
        ]


getInputSubs : Sub Msg
getInputSubs =
    Sub.map Input <| PlayerInput.subscriptions


type alias CanvasSize =
    ( Int, Int )


type Model
    = Initialising
    | UnrecoverableFail String
    | Good OkModel


type WhichScreen
    = SidePanel
    | GameEnd
    | Roaming


type alias OkModel =
    { size : CanvasSize
    , input : PlayerInput.Model
    , game : GameState
    , gui : WhichScreen
    }


type Msg
    = StartResponse (Result Http.Error InitialGameState)
    | ExpandResponse (Result Http.Error (Maybe Subgraph))
    | Resize Int Int
    | Input PlayerInput.Msg
    | EndScreen GameEndScreen.Msg
    | FinishRoaming
    | Tick Float


init : ( Model, Cmd Msg )
init =
    ( Initialising
    , getStart StartResponse
    )


getVpSize : Browser.Dom.Viewport -> Msg
getVpSize vp =
    Resize (floor vp.viewport.width) (floor vp.viewport.height)


guiElement : OkModel -> Element Msg
guiElement okm =
    case okm.gui of
        SidePanel ->
            sidePanel okm.game.focus

        GameEnd ->
            Element.map EndScreen endScreen

        Roaming ->
            Element.map (\_ -> FinishRoaming) finishRoamButton


view : Model -> Html Msg
view model =
    case model of
        Initialising ->
            text "Loading, please wait..."

        UnrecoverableFail why ->
            text <| "There was an error I can't recover from: " ++ why

        Good okm ->
            Element.layout [] <|
                Element.el
                    [ Element.inFront <|
                        guiElement okm
                    ]
                <|
                    Element.html (showGame okm)


showGame : OkModel -> Html Msg
showGame okm =
    let
        ( w, h ) =
            okm.size

        cam =
            okm.game.cam
    in
    Canvas.toHtml ( w, h )
        (PlayerInput.inputListenAttrs Input
            ++ [ style "display" "block"
               , style "box-sizing" "border-box"
               , PlayerInput.grabbyCursor okm.input
               ]
        )
        [ Canvas.shapes
            [ Canvas.Settings.fill Color.lightGrey ]
            [ Canvas.rect ( 0, 0 ) (toFloat w) (toFloat h) ]
        , renderGrid cam 100
        , Drawable.drawNavTree cam okm.game.nav
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

        Initialising ->
            case msg of
                StartResponse wd ->
                    handleStartResponse wd

                _ ->
                    ( Initialising, Cmd.none )

        Good okm ->
            case msg of
                Resize w h ->
                    updateGameState (SizeChanged <| Tuple.mapBoth toFloat toFloat ( w, h )) okm
                        |> Tuple.mapFirst (\nm -> Good { nm | size = ( w, h ) })

                Input i ->
                    let
                        ( newInputModel, inputCmds ) =
                            PlayerInput.update i okm.input
                    in
                    ( Good { okm | input = newInputModel }, Cmd.map Input inputCmds )

                Tick delta ->
                    tickGame delta okm

                StartResponse _ ->
                    ( model, Cmd.none )

                ExpandResponse resp ->
                    handleExpandResponse okm resp

                EndScreen GameEndScreen.Roaming ->
                    ( Good { okm | gui = Roaming }, Cmd.none )

                EndScreen GameEndScreen.NewGame ->
                    ( model, Cmd.none )

                FinishRoaming ->
                    ( Good { okm | gui = GameEnd }, Cmd.none )


updateGameState : GameState.Msg -> OkModel -> ( OkModel, Cmd Msg )
updateGameState msg okm =
    case GameState.update msg okm.game of
        GameOver ->
            ( { okm
                | gui = GameEnd
                , game = setRoaming okm.game
              }
            , Cmd.none
            )

        KeepGoing ( newGame, fetches ) ->
            ( { okm | game = newGame }
            , fetches
                |> List.map (\f -> postExpand (makeExpandParams f) ExpandResponse)
                |> Cmd.batch
            )


handleExpandResponse : OkModel -> Result Error (Maybe Subgraph) -> ( Model, Cmd Msg )
handleExpandResponse okm res =
    case res of
        Result.Err e ->
            ( UnrecoverableFail <| showErr e, Cmd.none )

        Ok Nothing ->
            ( UnrecoverableFail "Backend could not find requested node ID. This is a bug.", Cmd.none )

        Ok (Just sg) ->
            updateGameState (GotNodes <| xformSubgraph sg) okm
                |> Tuple.mapFirst Good


handleStartResponse : Result Http.Error InitialGameState -> ( Model, Cmd Msg )
handleStartResponse res =
    case res of
        Result.Err httpe ->
            ( UnrecoverableFail <| showErr httpe, Cmd.none )

        Result.Ok igs ->
            let
                initialCamera =
                    mkCamera ( 500, 500 )

                initialFocus =
                    getNode (xformSubgraph igs.subgraph) igs.startAt
                        |> Result.fromMaybe "Backend did not send content of initial node. This is a bug."

                goalObject =
                    getNode (xformSubgraph igs.subgraph) igs.endAt
                        |> Result.fromMaybe "Backend did not send content of the end node. This is a bug."
            in
            case Result.map2 Tuple.pair initialFocus goalObject of
                Result.Err s ->
                    ( UnrecoverableFail s
                    , Cmd.none
                    )

                Result.Ok ( startNode, endNode ) ->
                    ( Good
                        { size = ( 500, 500 )
                        , input = PlayerInput.init
                        , game = GameState.fromInitial startNode endNode initialCamera igs
                        , gui = SidePanel
                        }
                    , Task.perform getVpSize getViewport
                    )


tickGame : Float -> OkModel -> ( Model, Cmd Msg )
tickGame deltaMs m =
    let
        ( userInputs, resetInp ) =
            PlayerInput.consume m.input

        ( newModel, cmds ) =
            updateGameState (GotInputs userInputs) m

        ( newerModel, moreCmds ) =
            updateGameState (GameState.Tick deltaMs) newModel

        newestModel =
            Good { newerModel | input = resetInp }
    in
    ( newestModel, Cmd.batch [ cmds, moreCmds ] )
