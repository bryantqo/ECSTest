module PAM.UI.LeftSidebar exposing (..)

import Animation
import Element as El exposing (..)
import Element.Background as Bg
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Border as Border
import Html exposing (Html)
import Maybe.Extra as MEx
import PAM.UI.Basic as UI exposing (PanelDimensions, WindowSize, corners)
import PAM.UI.Palette as Palette



--
-- VIEW
--


view :
    Palette.PaletteSet ->
    { env
        | windowSize : WindowSize
        , device : Device
        , panelDimensions : PanelDimensions
        , sidePanelFx : Animation.State
        , sidePanelToggleFx : Animation.State
    }
    ->
        { config
            | onToggle : Maybe msg
            , header : Element msg
            , body : Element msg
            , footer : Element msg
        }
    -> Element msg
view palette env config =
    column
        (UI.renderElAnimation env.sidePanelFx
            [ height shrink
            , width shrink
            , alignLeft
            ]
        )
        [ row
            [ width fill
            , onRight <| toggleView palette env config.onToggle
            ]
            [ config.header ]
        , column
            [ height fill
            , width (px env.panelDimensions.leftSidebarWidth)
            ]
            [ config.body ]
        , row
            [ width fill ]
            [ config.footer ]
        ]


viewX :
    Palette.PaletteSet ->
    { env
        | windowSize : WindowSize
        , device : Device
        , size : { size | width : Length, height : Length }
        , sidePanelFx : Animation.State
        , sidePanelToggleFx : Animation.State
    }
    ->
        { config
            | onToggle : Maybe msg
            , header : Element msg
            , body : Element msg
            , footer : Element msg
        }
    -> Element msg
viewX palette env config =
    column
        ( UI.renderElAnimation env.sidePanelFx
            ( -- Palette.applyColorAttributes 
                -- Palette.Background 
                -- Palette.Primary 
                -- palette
                [ height env.size.height
                , width env.size.width
                , alignLeft
                , Border.shadow 
                    { offset = ( 9, 0 )
                    , size = 1
                    , blur = 8
                    , color = UI.fadeColor 0.2 UI.black
                    }
                , Bg.color <| UI.fadeColor 0.80 <| UI.white --rgb 0.7 0.7 0.7
                , Font.color <| rgb255 124 15 15
                , paddingEach { bottom = 0, left = 0, right = 0, top = 70 }
                , inFront 
                    <| row
                        [ width fill
                        --, inFront <| toggleViewX palette env config.onToggle
                        --, onRight <| el [ width <| px 20, height <| px 20 ] <| toggleViewX palette env config.onToggle
                        , paddingXY 0 10
                        ]
                        [ config.header
                        --, toggleViewX palette env config.onToggle 
                        ]
                ]
            )
        )
        [ column
            [ height fill
            , width (env.size.width)
            , clip
            , scrollbarY
            ]
            [ config.body ]
        , row
            [ width fill ]
            [ config.footer ]
        ]

toggleView :
    Palette.PaletteSet ->
    { env
        | windowSize : WindowSize
        , device : Device
        , panelDimensions : PanelDimensions
        , sidePanelToggleFx : Animation.State
    }
    -> Maybe msg
    -> Element msg
toggleView palette env onToggle =
    button
        [ Bg.color <| palette.action.primary.color
        , Font.color <| palette.action.primary.text
        , Font.size (UI.scaleFont env.windowSize 1)
        , height (px env.panelDimensions.leftSidebarHeaderHeight)
        , width (px env.panelDimensions.leftSidebarToggleWidth)
        , Border.roundEach { corners | bottomRight = 6 }
        , alignTop
        , alignRight
        , UI.title "Toggle Sidebar"
        ]
        { onPress = onToggle
        , label = UI.centeredIconWith "fa fa-chevron-right" env.sidePanelToggleFx
        }


toggleViewX :
    Palette.PaletteSet ->
    { env
        | windowSize : WindowSize
        , device : Device
        , size : { size | width : Length, height : Length }
        , sidePanelToggleFx : Animation.State
    }
    -> Maybe msg
    -> Element msg
toggleViewX palette env onToggle =
    button
        [ Bg.color <| palette.action.primary.color
        , Font.color <| palette.action.primary.text
        , Font.size (UI.scaleFont env.windowSize 1)
        , height (env.size.height)
        , width (px <| 40)
        , Border.roundEach { corners | bottomRight = 6 }
        , alignTop
        --, alignRight
        , UI.title "Toggle Sidebar"
        ]
        { onPress = onToggle
        , label = UI.centeredIconWith "fa fa-chevron-right" env.sidePanelToggleFx
        }