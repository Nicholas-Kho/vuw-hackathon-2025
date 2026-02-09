module SidePanel exposing (sidePanel)

import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , fill
        , fillPortion
        , height
        , htmlAttribute
        , none
        , padding
        , paragraph
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.BackendApi exposing (NodeContent)
import Html.Attributes


sidePanelContent : NodeContent -> NodeContent -> Element msg
sidePanelContent current end =
    column
        [ centerX
        , width fill
        , spacing 20
        ]
        [ showNodeInfo "Current focus" current
        , showNodeInfo "Target" end
        ]


showNodeInfo : String -> NodeContent -> Element msg
showNodeInfo t con =
    column
        [ width fill
        , Border.color (rgb255 110 110 110)
        , Border.rounded 12
        , Border.width 4
        , Background.color (rgb255 248 248 248)
        ]
        [ title t
        , subtitle con.title
        , paragraph [] [ text con.description ]
        ]


title : String -> Element msg
title str =
    el
        [ width fill
        , padding 10
        , centerX
        , Font.center
        , Font.size 32
        , Background.color <| rgb255 141 189 240
        , Border.roundEach
            { topLeft = 12
            , topRight = 12
            , bottomLeft = 0
            , bottomRight = 0
            }
        ]
        (text str)


subtitle : String -> Element msg
subtitle str =
    el
        [ width fill
        , padding 10
        , centerX
        , Font.center
        , Font.size 22
        , Background.color <| rgb255 167 199 231
        ]
        (text str)


showSidePanel : Element msg -> Element msg
showSidePanel stuff =
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
            htmlAttribute <| Html.Attributes.style "pointer-events" "none"

        clearBox attrs =
            el attrs none
    in
    row
        [ width fill
        , height fill
        , passThru
        ]
        [ clearBox [ width (fillPortion leftGapPart) ]
        , column [ width (fillPortion sidebarPart), height fill ]
            [ clearBox [ height (fillPortion verticalGapPart) ]
            , el
                [ width fill
                , height (fillPortion sidebarHtPart)
                , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
                ]
                stuff
            , clearBox [ height (fillPortion verticalGapPart) ]
            ]
        , clearBox [ width (fillPortion rightGapPart) ]
        ]


sidePanel : NodeContent -> NodeContent -> Element msg
sidePanel start end =
    showSidePanel <| sidePanelContent start end
