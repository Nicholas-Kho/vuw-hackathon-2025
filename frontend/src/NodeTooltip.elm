module NodeTooltip exposing
    ( Tooltip
    , Tooltips
    , diffTooltips
    , getTooltips
    , noTooltips
    , showTooltips
    )

import BackendWrapper exposing (Node, getContent, unwrapNodeId)
import Camera exposing (Camera, Vec2, vDistSqare, worldPosToCamPos)
import Dict exposing (Dict)
import Element exposing (px)
import Generated.BackendApi exposing (NodeId)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (attribute, class, classList, style)
import Html.Keyed as Keyed
import Navigation exposing (NTNode(..), NavTree, getLayout)
import Tree exposing (WithPos)


type alias Tooltip =
    { screenPos : Vec2
    , node : Node
    , active : Bool
    }



-- We need a wrapper because NodeIds aren't orderable, but they wrap strings which are.


type Tooltips
    = Tooltips (Dict String Tooltip)


noTooltips : Tooltips
noTooltips =
    Tooltips Dict.empty


getLoadedHelper : WithPos ( NodeId, NTNode ) -> Maybe ( NodeId, Tooltip )
getLoadedHelper wp =
    let
        ( nid, ntn ) =
            wp.content
    in
    case ntn of
        Fetching ->
            Nothing

        Loaded n ->
            Just ( nid, { screenPos = wp.pos, node = n, active = True } )


getTooltips : Camera -> NavTree -> Vec2 -> Tooltips
getTooltips cam nt cursorPosScreen =
    let
        appearRangeSquare =
            50 * 50

        inRangeAppear t =
            vDistSqare cursorPosScreen t.screenPos <= appearRangeSquare

        disableOutOfRange t =
            { t | active = inRangeAppear t }

        loadRangeSquare =
            200 * 200

        inRangeLoad t =
            vDistSqare cursorPosScreen t.screenPos <= loadRangeSquare
    in
    getLayout nt
        |> List.filterMap getLoadedHelper
        |> List.map (Tuple.mapSecond (\t -> { t | screenPos = worldPosToCamPos cam t.screenPos }))
        |> List.filter (Tuple.second >> inRangeLoad)
        |> List.map (Tuple.mapFirst unwrapNodeId)
        |> List.map (Tuple.mapSecond disableOutOfRange)
        |> Dict.fromList
        |> Tooltips


diffTooltips : Tooltips -> Tooltips -> Tooltips
diffTooltips (Tooltips old) (Tooltips new) =
    let
        oldOnly nid tooltip acc =
            Dict.insert nid { tooltip | active = False } acc

        both nid _ tt acc =
            Dict.insert nid tt acc

        newOnly nid tooltip acc =
            Dict.insert nid tooltip acc
    in
    Dict.merge oldOnly both newOnly old new Dict.empty |> Tooltips


tooltipAttrs : Tooltip -> List (Attribute msg)
tooltipAttrs tt =
    let
        px f =
            String.fromFloat f ++ "px"

        ( x, y ) =
            tt.screenPos

        styleString =
            "--x:" ++ px x ++ "; --y:" ++ px y ++ ";"
    in
    [ classList
        [ ( "node-tooltip", True )
        , ( "active", tt.active )
        ]
    , attribute "style" styleString
    ]


showTooltip : String -> Tooltip -> Html msg
showTooltip _ tt =
    div (tooltipAttrs tt)
        [ div [ class "node-tooltip-inner" ]
            [ text <| .title <| getContent tt.node ]
        ]


showTooltips : Tooltips -> Html msg
showTooltips (Tooltips ts) =
    ts
        |> Dict.toList
        |> List.map (\( nid, tt ) -> ( nid, showTooltip nid tt ))
        |> Keyed.node "div" [ style "position" "relative", class "tooltip-layer" ]
