module Game exposing (..)

import Browser
import GameState exposing (..)
import Generated.BackendApi exposing (InitialGameState, getStart)
import Html exposing (Html, text)
import Http exposing (Error(..))
import RemoteData exposing (RemoteData(..), WebData, fromResult)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    WebData InitialGameState


type Msg
    = StartResponse (WebData InitialGameState)


init : ( Model, Cmd Msg )
init =
    ( NotAsked, getStart (StartResponse << fromResult) )


view : Model -> Html Msg
view wd =
    case wd of
        RemoteData.NotAsked ->
            text "not asked"

        Loading ->
            text "loading"

        Failure err ->
            showErr err

        Success _ ->
            text "OK!"


showErr : Error -> Html Msg
showErr err =
    case err of
        NetworkError ->
            text "Network error"

        Timeout ->
            text "timeout"

        BadUrl s ->
            text s

        BadStatus s ->
            text <| String.fromInt <| s

        BadBody b ->
            text b


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        StartResponse wd ->
            ( wd, Cmd.none )
