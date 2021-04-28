module PAM.UI.BottomDrawer exposing (view)

import Animation
import Element as El exposing (..)
import Element.Background as Bg
import Element.Events as Ev exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html exposing (Html, i)
import Html.Attributes exposing (style)
import PAM.UI.Basic as UI exposing (PanelDimensions, WindowSize)
import PAM.UI.Palette as Palette



--
-- VIEW
--


view :
    Palette.PaletteSet ->
    { config
        | windowSize : WindowSize
        , panelDimensions : PanelDimensions
        , bottomPanelFx : Animation.State
        , bottomPanelToggleFx : Animation.State
        , riskLegendFx : Animation.State
    }
    -> msg
    -> List (Element msg)
    -> List (Element msg)
    -> Element msg
    -> Element msg
view palette config toggleMsg headerViews childViews legendView =
        column
            (UI.renderElAnimation config.bottomPanelFx
                [ height (px config.panelDimensions.bottomDrawerHeight)
                , width fill
                , alignBottom
                , Bg.color palette.background.primary.color
                , Font.color palette.background.primary.text
                , above 
                    <| row
                        (UI.renderElAnimation config.riskLegendFx
                            [ width fill
                            , height <| px 23
                            , Bg.color (rgba 0 0 0 0.8)
                            ] 
                        )
                        [ legendView 
                        ]
                
                ]
            )
            [ row 
                [ width fill 
                , El.htmlAttribute <| Html.Attributes.style "z-index" "40"
                ] 
                <|
                (List.reverse <|
                    toggleView palette config.windowSize toggleMsg config.bottomPanelToggleFx
                        :: headerViews
                )
            , column [ height fill, width fill ] childViews
            ]




toggleView : Palette.PaletteSet -> WindowSize -> msg -> Animation.State -> Element msg
toggleView palette windowSize msg fx =
    button
        [ --Bg.color UI.lunarGreen
        --, Font.color UI.white
        --, 
        Font.size (UI.scaleFont windowSize 1)
        , paddingXY 20 0
        , height fill
        , alignRight
        , UI.title "Toggle Grid"
        ]
        { onPress = Just msg
        , label = UI.centeredIconWith "fa fa-chevron-down" fx
        }
