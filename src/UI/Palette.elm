module PAM.UI.Palette exposing (..)

import Animation
import Color as Color
import Color.Manipulate as CEx
import Element exposing (..)
import Element.Background as Bg exposing (..)
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font exposing (..)
import Html as Html
import Html.Attributes as Attr
import Json.Decode as D exposing (Decoder)

import Json.Decode.Pipeline exposing (required, optional, hardcoded)


import PAM.UI.Basic as UI


type alias AppPalette =
    { header : PaletteSet
    , navigation : PaletteSet
    , content : PaletteSet
    , sidePanel : PaletteSet
    , modal : PaletteSet
    }

type alias PaletteSet =
    { background : Palette
    , action : Palette
    }

type PaletteSelector
    = Background
    | Action

type ColorSetSelector
    = Primary
    | Secondary
    | Tertiary
    | Quaternary

type alias Palette =
    { primary : ColorSet
    , secondary : ColorSet
    , tertiary : ColorSet
    , quaternary : ColorSet
    }

getPalette : PaletteSelector -> PaletteSet -> Palette
getPalette selector paletteSet =
    case selector of
        Background ->
            paletteSet.background
        Action ->
            paletteSet.action

getColorSet : ColorSetSelector -> Palette -> ColorSet
getColorSet selector palette =
    case selector of
        Primary ->
            palette.primary
        Secondary ->
            palette.secondary
        Tertiary ->
            palette.tertiary
        Quaternary ->
            palette.quaternary

getColorAttributes : PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg)
getColorAttributes palette color set =
    set 
    |> getPalette palette
    |> getColorSet color
    |> colorSetToAttributes 0 1

getColorAttributesDisabled : PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg)
getColorAttributesDisabled palette color set =
    set 
    |> getPalette palette
    |> getColorSet color
    |> colorSetToAttributes 0 0.5

applyColorAttributes : PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg) -> List (Attribute msg)
applyColorAttributes palette color set atts =
    atts
    ++ getColorAttributes palette color set

applyColorAttributesDisabled : PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg) -> List (Attribute msg)
applyColorAttributesDisabled palette color set atts =
    atts
    ++ getColorAttributesDisabled palette color set

applyBorderAttributes : Int -> PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg) -> List (Attribute msg)
applyBorderAttributes borderWidth palette color set atts =
    atts 
    ++ getBorderAttributes borderWidth palette color set

getColorAttributesActive : PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg)
getColorAttributesActive palette color set =
    set 
    |> getPalette palette
    |> getColorSet color
    |> colorSetToAttributes 0.15 1

applyColorAttributesActive : PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg) -> List (Attribute msg)
applyColorAttributesActive palette color set atts =
    atts
    ++ getColorAttributesActive palette color set


getBorderAttributes : Int -> PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg)
getBorderAttributes borderWidth palette color set =
    set 
    |> getPalette palette
    |> getColorSet color
    |> colorSetToBorderAttributes 0 borderWidth



colorSetToBorderAttributes : Float -> Int -> ColorSet -> List (Attribute msg)
colorSetToBorderAttributes darken borderWidth set =
    [ Border.color <| UI.darkenColor ( darken + 0.25 ) set.color
    , Border.width borderWidth
    ]

getColorAttributesHover : PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg)
getColorAttributesHover palette color set =
    set 
    |> getPalette palette
    |> getColorSet color
    |> colorSetToAttributes 0.30 1

applyColorAttributesHover : PaletteSelector -> ColorSetSelector -> PaletteSet -> List (Attribute msg) -> List (Attribute msg)
applyColorAttributesHover palette color set atts =
    atts
    ++ getColorAttributesHover palette color set

colorSetToAttributes : Float -> Float -> ColorSet -> List (Attribute msg)
colorSetToAttributes darken transparent set =
    [ Bg.color <| UI.fadeColor transparent <| UI.darkenColor darken set.color 
    , Font.color set.text
    ]

type alias ColorSet =
    { color : Color
    , text : Color
    }


fromSet : PaletteSet -> AppPalette
fromSet set =
    { header = set
    , navigation = set
    , content = set
    , sidePanel = set
    , modal = set
    }



appPaletteWithDefaultDecoder : AppPalette -> Decoder AppPalette
appPaletteWithDefaultDecoder default =
    D.succeed AppPalette
        |> optional "header" (paletteSetWithDefaultDecoder default.header) default.header
        |> optional "navigation" (paletteSetWithDefaultDecoder default.navigation) default.navigation
        |> optional "content" (paletteSetWithDefaultDecoder default.content) default.content
        |> optional "sidePanel" (paletteSetWithDefaultDecoder default.content) default.content
        |> optional "modal" (paletteSetWithDefaultDecoder default.modal) default.modal

paletteSetWithDefaultDecoder : PaletteSet -> Decoder PaletteSet
paletteSetWithDefaultDecoder default =
    D.succeed PaletteSet
        |> optional "background" ( paletteWithDefaultDecoder default.background )  default.background
        |> optional "action" ( paletteWithDefaultDecoder default.action ) default.action

paletteWithDefaultDecoder : Palette -> Decoder Palette
paletteWithDefaultDecoder default =
    D.succeed Palette
        |> optional "primary" (colorSetWithDefaultDecoder default.primary) default.primary
        |> optional "secondary" (colorSetWithDefaultDecoder default.secondary) default.secondary
        |> optional "tertiary" (colorSetWithDefaultDecoder default.tertiary) default.tertiary
        |> optional "quaternary" (colorSetWithDefaultDecoder default.quaternary) default.quaternary

colorSetWithDefaultDecoder : ColorSet -> Decoder ColorSet
colorSetWithDefaultDecoder default =
    D.succeed ColorSet
        |> optional "color" UI.rgb255ColorDecoder default.color
        |> optional "text" UI.rgb255ColorDecoder default.text




appPaletteDecoder : Decoder AppPalette
appPaletteDecoder =
    D.succeed AppPalette
        |> required "header" paletteSetDecoder
        |> required "navigation" paletteSetDecoder
        |> required "content" paletteSetDecoder
        |> required "sidePanel" paletteSetDecoder
        |> required "modal" paletteSetDecoder

paletteSetDecoder : Decoder PaletteSet
paletteSetDecoder =
    D.succeed PaletteSet
        |> required "background" paletteDecoder
        |> required "action" paletteDecoder

paletteDecoder : Decoder Palette
paletteDecoder =
    D.succeed Palette
        |> required "primary" colorSetDecoder
        |> required "secondary" colorSetDecoder
        |> required "tertiary" colorSetDecoder
        |> required "quaternary" colorSetDecoder

colorSetDecoder : Decoder ColorSet
colorSetDecoder =
    D.succeed ColorSet
        |> required "color" UI.rgb255ColorDecoder
        |> required "text" UI.rgb255ColorDecoder



defaultPalette =
    palePalette

palePalette =
    { header = standardPalette
    , navigation = standardPalette
    , content = standardPalette
    , sidePanel = standardPalette
    , modal = standardPalette
    }

utPalette =
    { header = utahPalette
    , navigation = utahPaletteDetails
    , content = utahPaletteGrid
    , sidePanel = utahPaletteGrid
    , modal = utahPalette
    }

standardPalette : PaletteSet
standardPalette =
    { background = defaultBackground
    , action = defaultAction
    }

defaultBackground : Palette
defaultBackground =
    { primary = { color = UI.doveGray, text = UI.white }
    , secondary = { color = UI.satinLinen, text = UI.codGray }
    , tertiary = { color = UI.darkenColor 0.25 UI.willowGrove, text = UI.white }
    , quaternary = { color = UI.willowGrove, text = UI.black }
    }

defaultAction : Palette
defaultAction =
    { primary = { color = UI.darkenColor 0.75 UI.white , text = UI.satinLinen }
    , secondary = { color = UI.darkenColor 0.5 UI.white, text = UI.satinLinen }
    , tertiary = { color = UI.darkenColor 0.25 UI.white, text = UI.white }
    , quaternary = { color = UI.white, text = UI.black }
    }







utahPalette : PaletteSet
utahPalette =
    { background = utahBackground
    , action = utahAction
    }

utahPaletteDetails : PaletteSet
utahPaletteDetails =
    { background = utahBackground
    , action = utahActionDetails
    }

utahPaletteGrid : PaletteSet
utahPaletteGrid =
    { background = utahBackgroundGrid
    , action = utahActionGrid
    }

utahBackground : Palette
utahBackground =
    { primary = { color = UI.pineTree, text = UI.white }
    , secondary = { color = UI.asparagus, text = UI.pineTree }
    , tertiary = { color = UI.doveGray, text = UI.orangeRoughy }
    , quaternary = { color = UI.codGray, text = UI.white }
    }

utahBackgroundGrid : Palette
utahBackgroundGrid =
    { primary = { color = UI.pineTree, text = UI.white }
    , secondary = { color = UI.white, text = UI.black }
    , tertiary = { color = UI.doveGray, text = UI.orangeRoughy }
    , quaternary = { color = UI.codGray, text = UI.white }
    }

utahActionGrid : Palette
utahActionGrid =
    { primary = { color = UI.orangeRoughy, text = UI.white }
    , secondary = { color = UI.woodland, text = UI.white }
    , tertiary = { color = UI.greenSmoke, text = UI.greenSmoke }
    , quaternary = { color = UI.pineTree, text = UI.white }
    }

utahActionDetails : Palette
utahActionDetails =
    { primary = { color = UI.orangeRoughy, text = UI.white }
    , secondary = { color = UI.asparagus, text = UI.white }
    , tertiary = { color = UI.pineTree, text = UI.white }
    , quaternary = { color = UI.white, text = UI.black }
    }

utahAction : Palette
utahAction =
    { primary = { color = UI.orangeRoughy, text = UI.white }
    , secondary = { color = UI.greenSmoke, text = UI.white }
    , tertiary = { color = UI.woodland, text = UI.greenSmoke }
    , quaternary = { color = UI.pineTree, text = UI.white }
    }






primaryActionBackground : Color 
primaryActionBackground =
    UI.orangeRoughy

secondaryActionBackground : Color 
secondaryActionBackground =
    UI.pineTree