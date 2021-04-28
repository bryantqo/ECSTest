module PAM.UI.StandardElements exposing (..)

import Element exposing (..)
import Element.Background as Bg exposing (..)
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font

import PAM.UI.Basic as UI
import PAM.UI.Palette as Palette exposing (PaletteSet)



type ButtonType 
    = PrimaryAction
    | SecondaryAction
    | TertiaryAction







buttonTypeColor : ButtonType -> PaletteSet -> Color
buttonTypeColor buttonType palette =
    case buttonType of
        PrimaryAction ->
            palette.action.primary.color
        SecondaryAction ->
            palette.action.secondary.color
        TertiaryAction ->
            palette.action.tertiary.color

buttonTextColor : ButtonType -> PaletteSet -> Color
buttonTextColor buttonType palette =
    case buttonType of
        PrimaryAction ->
            palette.action.primary.text
        SecondaryAction ->
            palette.action.secondary.text
        TertiaryAction ->
            palette.action.tertiary.text


getFontScaleFor : ButtonType -> Int 
getFontScaleFor buttonType =
    case buttonType of
        PrimaryAction ->
            1
        SecondaryAction ->
            1
        TertiaryAction ->
            -1

button : ButtonType 
    ->  { env 
        | windowSize : UI.WindowSize
        , device : Device
        , palette : PaletteSet 
        } 
    -> List (Attribute msg) 
    ->  { label : List (Element msg)
        , onPress : Maybe msg
        } 
    -> Element msg
button buttonType env addlAttrs settings =
    let
        fade = 
            if settings.onPress /= Nothing then
                1 
            else 
                0.5
    in
    Input.button 
        ( [ Bg.color <| UI.fadeColor fade <| buttonTypeColor buttonType env.palette
        , Font.color <| UI.fadeColor fade <| buttonTextColor buttonType env.palette
        , Font.bold
        , Font.size 
            <| UI.scaleFont env.windowSize 
            <| getFontScaleFor buttonType 
        , height <| UI.inputHeight env
        , UI.montserrat
        , paddingXY 25 0
        --, width <| minimum 150 shrink
        , Border.rounded 16
        ]
        ++ 
        addlAttrs
        )
        { onPress = settings.onPress
        , label = 
            el
                [ centerX
                , centerY
                , width <| minimum 150 shrink
                ]
            <| row 
                [ spacing 3
                , centerX
                ]
                settings.label
        }
    