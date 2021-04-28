module PAM.UI.Tooltip exposing (..)

import Element as El exposing (..)
import Element.Keyed as Keyed
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


tooltipInFront : String -> Attribute msg
tooltipInFront txt =
   tooltipRender inFront [ centerX, centerY ] (tooltipText txt)

tooltipCustom : Element Never -> Attribute msg
tooltipCustom ele =
   tooltipRender onLeft [ centerX, centerY ] ele

tooltipBelow : String -> Attribute msg
tooltipBelow txt =
   tooltipRender below [ centerX ] (tooltipText txt)

tooltipAbove : String -> Attribute msg
tooltipAbove txt =
   tooltipRender above [ centerX ] (tooltipText txt)

tooltipLeft : String -> Attribute msg
tooltipLeft txt =
   tooltipRender onLeft [ centerY ] (tooltipText txt)
   
tooltipLeftBelow : String -> Attribute msg
tooltipLeftBelow txt =
   tooltipRender onLeft [ ] ( column [ ] [ el [ height <| px 25 ] none, tooltipText txt ] )

tooltipLeftAbove : String -> Attribute msg
tooltipLeftAbove txt =
   tooltipRender above [padding 5, onLeft <| tooltipText txt] (el [ transparent True, width <| px 5, clip, htmlAttribute (Html.Attributes.style "pointerEvents" "none") ] <| tooltipText txt)

tooltipRight : String -> Attribute msg
tooltipRight txt =
   tooltipRender onRight [ centerY ] (tooltipText txt)

tooltipText : String -> Element msg
tooltipText str =
    el
        [ Bg.color (rgba 0 0 0 0.75)
        , Font.color (rgb 1 1 1)
        , paddingXY 5 7
        , Border.rounded 5
        , Font.size 12
        , Border.shadow
            { offset = ( 0, 3 )
            , blur = 6
            , size = 0
            , color = rgba 0 0 0 0.32 
            }
        , htmlAttribute (Html.Attributes.style "pointerEvents" "none")
        ]
        <| text str

tooltipRender : (Element msg -> Attribute msg) -> List (Attribute Never) -> Element Never -> Attribute msg
tooltipRender usher attrs tooltip_ =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , (usher << El.map never) <|
                el ( [ htmlAttribute (Html.Attributes.style "pointerEvents" "none")] ++ attrs )
                    tooltip_
            ]
            none