module GameEndScreen exposing (endScreen)

import Element
    exposing
        ( Attribute
        , Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , padding
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


showMidPanel : Element msg -> Element msg
showMidPanel content =
    el [ height fill, width fill, padding 20 ] content


screenContent : Element msg
screenContent =
    column
        [ Background.color (rgb255 248 248 248)
        , width fill
        , height fill
        , Border.color (rgb255 110 110 110)
        , Border.rounded 12
        , Border.width 4
        , padding 10
        , spacing 50
        ]
        [ el
            [ Font.center
            , Font.size 64
            , width fill
            , centerY
            ]
          <|
            text "You won!"
        , row [ centerX, centerY, spacing 20, padding 20 ]
            [ prettyButton []
                { onPress = Nothing
                , label = "Keep roaming"
                }
            , prettyButton []
                { onPress = Nothing
                , label = "New game"
                }
            ]
        ]


prettyButton :
    List (Attribute msg)
    -> { onPress : Maybe msg, label : String }
    -> Element msg
prettyButton extraAttrs props =
    let
        decorations =
            [ Background.color <| rgb255 167 199 231
            , Border.color (rgb255 110 110 110)
            , Border.rounded 12
            , Border.width 4
            ]

        label =
            el [ padding 20 ] (text props.label)
    in
    Input.button (decorations ++ extraAttrs)
        { onPress = props.onPress
        , label = label
        }


endScreen : Element msg
endScreen =
    showMidPanel <| screenContent
