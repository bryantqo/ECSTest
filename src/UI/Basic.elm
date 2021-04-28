module UI.Basic exposing (..)

import Animation
import Color as Color
import Color.Manipulate as CEx
import Element exposing (..)
import Element.Background as Bg exposing (..)
import Element.Border as Border
import Element.Input as Input
import Element.Font exposing (..)
import Html as Html
import Html.Attributes as Attr
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)


type alias WindowSize =
    { width : Int
    , height : Int
    }


rgb255ColorDecoder : Decoder Color
rgb255ColorDecoder =
    D.map3
        rgb255
        (D.index 0 D.int)
        (D.index 1 D.int)
        (D.index 2 D.int)


rgba255ColorDecoder : Decoder Color
rgba255ColorDecoder =
    D.map4
        rgba255
        (D.index 0 D.int)
        (D.index 1 D.int)
        (D.index 2 D.int)
        (D.index 3 D.float)

windowSizeDecoder : Decoder WindowSize
windowSizeDecoder =
    D.map2 WindowSize
        (D.field "width" D.int)
        (D.field "height" D.int)


type Openness
    = Open
    | Closed


flipOpenness : Openness -> Openness
flipOpenness openness =
    case openness of
        Open ->
            Closed

        Closed ->
            Open


isOpen : Openness -> Bool
isOpen openness =
    case openness of
        Open ->
            True

        Closed ->
            False


isClosed : Openness -> Bool
isClosed openness =
    case openness of
        Open ->
            False

        Closed ->
            True


opennessFromBool : Bool -> Openness
opennessFromBool val =
    case val of
        True ->
            Open

        False ->
            Closed


type alias PanelDimensions =
    { mainHeaderHeight : Int
    , subHeaderHeight : Int
    , bottomDrawerHeaderHeight : Int
    , bottomDrawerHeight : Int
    , bottomDrawerFooterHeight : Int
    , leftSidebarToggleWidth : Int
    , leftSidebarWidth : Int
    , leftSidebarHeaderHeight : Int
    , leftSidebarBodyHeight : Int
    , leftSidebarFooterHeight : Int
    }


calculatePanelDimensions : WindowSize -> Device -> Int -> PanelDimensions
calculatePanelDimensions windowSize device adjustedHeight =
    let
        calc =
            adjustOnHeight windowSize device

        mainHeaderHeight =
            calc (50, 60)

        subHeaderHeight =
            calc (30, 40)

        headerAndFooterHeight =
            calc ( 32, 46 )

        bottomDrawerHeight = 
            calc ( 215, 316 )

        leftSidebarBodyHeight =
            -- windowSize.height - (mainHeaderHeight + subHeaderHeight + headerAndFooterHeight*2)
            windowSize.height - (mainHeaderHeight + subHeaderHeight + headerAndFooterHeight*adjustedHeight)

    in
    { mainHeaderHeight = mainHeaderHeight
    , subHeaderHeight = subHeaderHeight
    , bottomDrawerHeaderHeight = headerAndFooterHeight
    , bottomDrawerHeight = bottomDrawerHeight
    , bottomDrawerFooterHeight = calc ( 32, 40 )
    , leftSidebarToggleWidth = calc ( 30, 40 )
    , leftSidebarWidth = calc ( 390, 480 )
    , leftSidebarHeaderHeight = headerAndFooterHeight
    , leftSidebarBodyHeight = leftSidebarBodyHeight
    , leftSidebarFooterHeight = headerAndFooterHeight
    }



-- PALETTE

asparagus : Color
asparagus =
    rgb255 130 151 80


greenSmoke : Color
greenSmoke =
    rgb255 154 172 109


woodland : Color
woodland =
    rgb255 78 90 48


willowGrove : Color
willowGrove =
    rgb255 105 111 102


woodland48 : Color
woodland48 =
    rgba255 78 90 48 0.48


greenKelp : Color
greenKelp =
    rgb255 53 61 34


orangeRoughy : Color
orangeRoughy =
    rgb255 203 77 21


oregon : Color
oregon =
    rgb255 154 47 0


pineTree : Color
pineTree =
    rgb255 32 61 5


doveGray : Color
doveGray =
    rgb255 103 103 103


satinLinen : Color
satinLinen =
    rgb255 230 232 214


white : Color
white =
    rgb255 255 255 255

redish : Color
redish =
    rgb 1 0 0


black : Color
black =
    rgb255 0 0 0


lunarGreen : Color
lunarGreen =
    rgb255 63 67 61


codGray : Color
codGray =
    rgb255 23 23 23


earlyDawn : Color
earlyDawn =
    rgb255 255 250 235

azureRadiance : Color
azureRadiance =
    rgb255 0 126 255


convertColor : Color.Color -> Color
convertColor color =
    Color.toRgba color
        |> (\{ red, green, blue, alpha } ->
                rgba red green blue alpha
           )


fadeColor : Float -> Color -> Color
fadeColor alpha color =
    let
        c = toRgb color
    in
    rgba c.red c.green c.blue alpha


darkenColor : Float -> Color -> Color
darkenColor val color =
    let
        c = toRgb color
    in
    Color.fromRgba c
        |> CEx.darken val
        |> convertColor


transparentN : Float -> Color
transparentN alpha =
    rgba 0 0 0 (abs alpha)


transparent0 : Color
transparent0 =
    transparent 0.0


transparent25 : Color
transparent25 =
    transparent 0.25


transparent40 : Color
transparent40 =
    transparent 0.4


transparent50 : Color
transparent50 =
    transparent 0.5


transparent70 : Color
transparent70 =
    transparent 0.7

transparent75 : Color
transparent75 =
    transparent 0.75

transparent85 : Color
transparent85 =
    transparent 0.85


transparent : Float -> Color
transparent pct =
    rgba255 0 0 0 pct


type alias Corners =
    { topLeft : Int
    , topRight : Int
    , bottomLeft : Int
    , bottomRight : Int
    }


corners : Corners
corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


type alias Edges =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


edges : Edges
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


type alias Borders =
    { bottom : Int
    , left : Int
    , right : Int
    , top : Int
    }


borders : Borders
borders =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


helvetica : Attribute msg
helvetica =
    family
        [ typeface "Helvetica"
        , sansSerif
        ]


montserrat : Attribute msg
montserrat =
    family
        [ typeface "Montserrat"
        , sansSerif
        ]


title : String -> Attribute msg
title data =
    htmlAttribute <| Attr.attribute "title" data


class : String -> Attribute msg
class classname =
    htmlAttribute <| Attr.attribute "class" classname


style : String -> String -> Attribute msg
style name value =
    htmlAttribute <| Attr.style name value


id : String -> Attribute msg
id idname =
    htmlAttribute <| Attr.attribute "id" idname


icon : String -> Element msg
icon className =
    el
        [ width (fill) -- |> minimum 14)
        , height (fill) -- |> minimum 14)
        , centerX
        , centerY
        ]
    <|
        html <|
            Html.i [ Attr.class className ] []


iconWith : String -> Animation.State -> Element msg
iconWith className fx =
    el
        (   [ width (shrink |> minimum 14)
            , height (shrink |> minimum 14)
            ]
            ++ 
            ( List.map htmlAttribute <| renderAnimation fx [] )
        )
    <|
        html <|
            Html.i (renderAnimation fx [ Attr.class className ]) []


limitText : String -> String
limitText value =
    let
        clip = 39
    in
    if String.length value > clip then
        (String.left (clip) value) ++ "..."
    else
        value


centeredIcon : String -> Element msg
centeredIcon className =
    el 
        [ width fill
        , height fill 
        ]
    <| icon className


modalShadow : List (Attribute msg) -> List (Attribute msg)
modalShadow =
    (++)
    [ Border.shadow
        { color = black 
        , size = 2
        , blur = 10
        , offset = (5,5)
        }
    ]

centeredIconWith : String -> Animation.State -> Element msg
centeredIconWith className fx =
    el [ centerX, centerY ] <| iconWith className fx

centeredIconWithBonus : List (Attribute msg) -> String -> Animation.State -> Element msg
centeredIconWithBonus attrs className fx =
    el 
        (
            [ centerX
            , centerY 
            ]
        ) 
    <| iconWith className fx


checkboxIcon : Bool -> Element msg
checkboxIcon checked =
    case checked of
        True ->
            icon "fa fa-check-square-o"

        False ->
            icon "fa fa-square-o"



--
-- RESPONSIVE HELPERS
--


is1080p : WindowSize -> Bool
is1080p size =
    size.width >= 1920 || size.height >= 1080


is720p : WindowSize -> Bool
is720p size =
    (size.width >= 1280 || size.height >= 720) && not (is1080p size)


inputHeight : { config | windowSize : WindowSize, device : Device } -> Length
inputHeight { windowSize, device } =
    px <| adjustOnHeight windowSize device ( 26, 30 )




{-| Using a default font height of between 13 and 16 (depending on
screen resolution) and a default scaling ratio of 1.15, and multiplies
them by a given number of times.

    p1080 = { width = 1920, height = 1080 }

    p720 = { width = 1280, height = 720 }

    Font.size (scaleFont p1080 -1) -- results in 14

    Font.size (scaleFont p720 -1) -- results in 12

    Font.size (scaleFont p1080 1) -- results in 16

    Font.size (scaleFont p720 1) -- results in 13

    Font.size (scaleFont p1080 2) -- 16 * 1.15 results in 18

    Font.size (scaleFont p720 2) -- 13 * 1.15 results in 15

    Font.size (scaleFont p1080 4) -- 16 * 1.15 ^ (4 - 1) results in 24

    Font.size (scaleFont p720 4) -- 13 * 1.15 ^ (4 - 1) results in 20

-}
scaleFont : WindowSize -> Int -> Int
scaleFont size factor =
    let
        baseFont =
            responsiveFont (toFloat size.width) ( 500, 3850 ) ( 8, 30 )
    in
    round <| modular baseFont 1.15 factor


scaleWidth : WindowSize -> Float -> Float -> Int
scaleWidth size from to =
    let
        baseFont =
            responsiveFont (toFloat size.width) ( 500, 3850 ) ( from, to )
    in
    round <|  baseFont --modular baseFont 1.15 1


{-| Takes a desired range, the current window size, and the device to
determine an adjusted height value when the window height is
between 600 and 1200. Returns the lower or upper bound respectively
when window height exceeds the default range.

    Note: Not currently using the Device, but we may change that

-}
adjustOnHeight : WindowSize -> Device -> ( Int, Int ) -> Int
adjustOnHeight size device range =
    responsive size.height ( 600, 1200 ) range


{-| Takes a desired range, the current window size, and the device to
determine an adjusted width value when the window width is
between 1200 and 1800. Returns the lower or upper bound respectively
when window width exceeds the default range.

    Note: Not currently using the Device, but we may change that

-}
adjustOnWidth : WindowSize -> Device -> ( Int, Int ) -> Int
adjustOnWidth size device range =
    responsive size.width ( 1200, 1800 ) range


{-| Define two ranges that should linearly match up with each other

    Provide a value for the first and receive the calculated value for the second.

        fontsize =
            responsiveFont windowSize.width ( 1200, 1800 ) ( 9, 16 )

    When the device width is between 1200 and 1800, set the base font size
    between 9 and 16 using a linear scale.

    Generally, you should reach for `scaleFont` first.

    Note: Modified from mdgriffith/style-elements since it was removed from elm-ui

    :shrug:

-}
responsiveFont : Float -> ( Float, Float ) -> ( Float, Float ) -> Float
responsiveFont a ( aMin, aMax ) ( bMin, bMax ) =
    if a <= aMin then
        bMin

    else if a >= aMax then
        bMax

    else
        let
            deltaA =
                (a - aMin) / (aMax - aMin)
        in
        (deltaA * (bMax - bMin)) + bMin


{-| Define two ranges that should linearly match up with each other

    Provide a value for the first and receive the calculated value for the second.

        elWidth =
            responsive windowSize.width ( 600, 1200 ) ( 16, 20 )

    When the device width is between 600 and 1200, set the element width
    between 16 and 20 using a linear scale (rounded to nearest whole num).

    Generally, you should reach first for `adjustOnWidth` and sometimes `adjustOnHeight`

    Note: Modified from mdgriffith/style-elements since it was removed from elm-ui

    :shrug:

-}
responsive : Int -> ( Int, Int ) -> ( Int, Int ) -> Int
responsive a ( aMin, aMax ) ( bMin, bMax ) =
    if a <= aMin then
        bMin

    else if a >= aMax then
        bMax

    else
        let
            deltaA =
                round <| (toFloat <| a - aMin) / (toFloat <| aMax - aMin)
        in
        (deltaA * (bMax - bMin)) + bMin



--
-- ANIMATIONS
--


renderAnimation : Animation.State -> List (Html.Attribute msg) -> List (Html.Attribute msg)
renderAnimation animations otherAttrs =
    Animation.render animations ++ otherAttrs


renderElAnimation : Animation.State -> List (Attribute msg) -> List (Attribute msg)
renderElAnimation animations otherAttrs =
    (List.map htmlAttribute <| Animation.render animations) ++ otherAttrs


type alias ToggleStates =
    { rotateZero : List Animation.Property
    , rotateNeg180 : List Animation.Property
    , rotate180 : List Animation.Property
    }


toggleStates : ToggleStates
toggleStates =
    { rotateZero =
        [ Animation.opacity 1
        , Animation.backgroundColor { red = 255, green = 255, blue = 255, alpha = 1 }
        ]
    , rotateNeg180 =
        [ Animation.opacity 0
        , Animation.backgroundColor { red = 255, green = 255, blue = 255, alpha = 0 }
        ]
    , rotate180 =
        [ Animation.rotate (Animation.deg 180.0) 
        ]
    }


type alias LeftSidebarStates =
    { open : List Animation.Property
    , closed : List Animation.Property
    , full : List Animation.Property
    , short : List Animation.Property
    }


leftSidebarStates : PanelDimensions -> LeftSidebarStates
leftSidebarStates dims =
    let
        lsWidth =
            toFloat dims.leftSidebarWidth

        lsbHeight = 
            toFloat dims.leftSidebarBodyHeight

        bdhHeight =
            toFloat dims.bottomDrawerHeaderHeight

        bdHeight =
            toFloat dims.bottomDrawerHeight
    in
    { open =
        [ Animation.left (Animation.px 0.0)
        , Animation.opacity 1.0
        ]
    , closed =
        [ Animation.left (Animation.px -lsWidth)
        , Animation.opacity 0
        ]
    , full =
        [ Animation.height (Animation.px <| lsbHeight - bdhHeight)
        ]
    , short =
        [ Animation.height (Animation.px <| lsbHeight - bdHeight)
        ]
    }


leftSidebarStatesX : { size | width : Int, height : Int } -> LeftSidebarStates
leftSidebarStatesX { width, height } =
    let
        lsWidth =
            toFloat ( width + 40 )

        lsbHeight = 
            toFloat height
    in
    { open =
        [ Animation.left (Animation.px 0.0)
        ]
    , closed =
        [ Animation.left (Animation.px -lsWidth)
        ]
    , full =
        [ Animation.height (Animation.px <| lsbHeight)
        ]
    , short =
        [ Animation.height (Animation.px <| lsbHeight)
        ]
    }

type alias BottomDrawerStates =
    { open : List Animation.Property
    , closed : List Animation.Property
    }


bottomDrawerStates : PanelDimensions -> BottomDrawerStates
bottomDrawerStates dims =
    let
        bdHeight =
            toFloat <| dims.bottomDrawerHeight - dims.bottomDrawerHeaderHeight
    in
    { open =
        [ Animation.bottom (Animation.px 0.0) ]
    , closed =
        [ Animation.bottom (Animation.px -bdHeight) ]
    }


type alias RiskLegendStates =
    { full : List Animation.Property
    , short : List Animation.Property
    }


riskLegendStates : WindowSize -> PanelDimensions -> RiskLegendStates
riskLegendStates windowSize dims =
    let
        winWidth =
            toFloat windowSize.width

        lsWidth =
            toFloat dims.leftSidebarWidth

        rlWidth =
            winWidth - lsWidth
    in
    { full =
        [ Animation.width (Animation.px winWidth)
        , Animation.left (Animation.px 0)
        ]
    , short =
        [ Animation.width (Animation.px rlWidth)
        , Animation.left (Animation.px lsWidth)
        ]
    }
