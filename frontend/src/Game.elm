module Game exposing (..)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Camera exposing (mkCamera)
import Canvas
import Canvas.Settings
import Color
import Drawable exposing (drawMNodeTree, renderGrid)
import Element exposing (Element, fillPortion, layout)
import Element.Background
import Element.Border
import GameState exposing (..)
import Generated.BackendApi exposing (InitialGameState, getStart)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Http exposing (Error(..))
import Navigation
import Platform.Sub exposing (none)
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
        , onAnimationFrameDelta Tick
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
    | Tick Float


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
            layout [] <|
                Element.el [ Element.inFront (showSidePanel okm) ] <|
                    Element.html (showGame okm)


showSidePanel : OkModel -> Element Msg
showSidePanel okm =
    let
        leftGapPart =
            1

        sidebarPart =
            7

        rightGapPart =
            24

        sidebarHtPart =
            30

        verticalGapPart =
            1

        passThru =
            Element.htmlAttribute <| Html.Attributes.style "pointer-events" "none"

        clearBox attrs =
            Element.el attrs Element.none
    in
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , passThru
        ]
        [ clearBox [ Element.width (fillPortion leftGapPart) ]
        , Element.column [ Element.width (fillPortion sidebarPart), Element.height Element.fill ]
            [ clearBox [ Element.height (fillPortion verticalGapPart) ]
            , Element.column
                [ Element.width Element.fill
                , Element.height (fillPortion sidebarHtPart)
                , Element.Border.color (Element.rgb255 110 110 110)
                , Element.Border.rounded 12
                , Element.Border.width 4
                , Element.Background.color (Element.rgb255 248 248 248)
                , Element.htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
                ]
                [ Element.none ]
            , clearBox [ Element.height (fillPortion verticalGapPart) ]
            ]
        , clearBox [ Element.width (fillPortion rightGapPart) ]
        ]


showGame : OkModel -> Html Msg
showGame okm =
    let
        ( w, h ) =
            okm.size

        cam =
            okm.game.cam
    in
    Canvas.toHtml ( w, h )
        [ style "display" "block"
        , style "box-sizing" "border-box"
        , PlayerInput.grabbyCursor okm.input
        , PlayerInput.scrollAttr Input
        ]
        [ Canvas.shapes
            [ Canvas.Settings.fill Color.lightGrey ]
            [ Canvas.rect ( 0, 0 ) (toFloat w) (toFloat h) ]
        , renderGrid cam 100
        , drawMNodeTree cam (Navigation.toNodeTree okm.game.graph okm.game.nav)
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
                    let
                        ( newInputModel, inputCmds ) =
                            PlayerInput.update i okm.input
                    in
                    ( Good { okm | input = newInputModel }, Cmd.map Input inputCmds )

                Tick delta ->
                    ( Good (tickGame delta okm), Cmd.none )

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


tickGame : Float -> OkModel -> OkModel
tickGame deltaMs m =
    let
        ( userInputs, resetInp ) =
            PlayerInput.consume m.input
    in
    { m
        | input = resetInp
        , game = handleTick deltaMs <| handleInputs userInputs m.game
    }
