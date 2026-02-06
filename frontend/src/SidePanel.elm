module SidePanel exposing (sidePanel)

import Element
    exposing
        ( Element
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , htmlAttribute
        , none
        , paragraph
        , rgb255
        , row
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Generated.BackendApi exposing (NodeContent)
import Html.Attributes


sidePanelContent : NodeContent -> Element msg
sidePanelContent con =
    column [ centerY ]
        [ text <| "Current focus: " ++ con.title
        , paragraph [] [ text con.description ]
        ]


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
                , Border.color (rgb255 110 110 110)
                , Border.rounded 12
                , Border.width 4
                , Background.color (rgb255 248 248 248)
                , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
                ]
                stuff
            , clearBox [ height (fillPortion verticalGapPart) ]
            ]
        , clearBox [ width (fillPortion rightGapPart) ]
        ]


sidePanel : NodeContent -> Element msg
sidePanel =
    showSidePanel << sidePanelContent
