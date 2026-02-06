module FinishRoamButton exposing (finishRoamButton)

import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , el
        , fill
        , height
        , htmlAttribute
        , padding
        , rgb255
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes


finishRoamButton : Element ()
finishRoamButton =
    el
        [ htmlAttribute <| Html.Attributes.style "pointer-events" "none"
        , width fill
        , height fill
        , padding 20
        ]
        buttonContent


buttonContent : Element ()
buttonContent =
    Input.button
        [ htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
        , padding 20
        , alignBottom
        , alignLeft
        , Background.color <| rgb255 167 199 231
        , Border.color (rgb255 110 110 110)
        , Border.rounded 12
        , Border.width 4
        ]
        { onPress = Just ()
        , label = text "Done"
        }
