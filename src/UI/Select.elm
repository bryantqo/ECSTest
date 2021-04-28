module PAM.UI.Select exposing
    ( ButtonConfig
    , LabelConfig
    , OptionConfig
    , OptionListConfig
    , SelectConfig
    , SelectDirection(..)
    , defaultButtonConfig
    , defaultOptionConfig
    , defaultOptionListConfig
    , defaultSelectConfig
    , select
    )

import Browser.Dom as Dom
import Element as El exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import PAM.UI.Basic as UI exposing (Borders, Corners, Edges, Openness(..), PanelDimensions, WindowSize, borders, corners, edges)
import PAM.UI.Palette as Palette
import Task



--
-- SELECT TYPES
--


type SelectDirection
    = SelectDown
    | SelectUp


type alias OptionConfig a msg =
    { width : Length
    , height : Length
    , mouseOver : List Decoration
    , padding : Edges
    , otherAttrs : List (Attribute msg)
    , idleAttrs : List (Attribute msg)
    , focusedAttrs : List (Attribute msg)
    , selectedAttrs : List (Attribute msg)
    , titleGetter : a -> String
    }


type alias OptionListConfig a msg =
    { id : String
    , padding : Edges
    , height : Length
    , fontSize : Int
    , fontColor : Color
    , bgColor : Color
    , otherOuterAttrs : List (Attribute msg)
    , otherInnerAttrs : List (Attribute msg)
    , onLoseFocus : msg
    , onChange : a -> msg
    , selected : Maybe a
    , label : String
    }


type alias LabelConfig msg =
    { otherAttrs : List (Attribute msg)
    , label : String
    , openIcon : Maybe String
    , closedIcon : Maybe String
    }


type alias ButtonConfig msg =
    { id : String
    , width : Length
    , height : Length
    , padding : Edges
    , fontSize : Int
    , fontColor : Color
    , bgColor : Color
    , otherAttrs : List (Attribute msg)
    , onPress : Maybe msg
    }


type alias SelectConfig a msg =
    { buttonConfig : ButtonConfig msg
    , labelConfig : LabelConfig msg
    , optionListConfig : OptionListConfig a msg
    , optionConfig : OptionConfig a msg
    , otherAttrs : List (Attribute msg)
    , direction : SelectDirection
    , borderRadius : Int
    }



--
-- DEFAULT CONFIGS
--


defaultOptionConfig : WindowSize -> { args | titleGetter : a -> String } -> OptionConfig a msg
defaultOptionConfig size args =
    { width = fill
    , height = fill
    , mouseOver = [ Bg.color UI.greenKelp ]
    , padding = Edges 8 6 8 6
    , otherAttrs = []
    , idleAttrs = []
    , focusedAttrs = [ Bg.color UI.greenKelp ]
    , selectedAttrs = [ Bg.color UI.woodland ]
    , titleGetter = args.titleGetter
    }


defaultOptionListConfig :
    { config
        | device : Device
        , windowSize : WindowSize
    }
    ->
        { args
            | id : String
            , onLoseFocus : msg
            , onChange : a -> msg
            , selected : Maybe a
            , label : String
        }
    -> OptionListConfig a msg
defaultOptionListConfig config args =
    let
        maxHeight =
            -- config.panelDimensions.bottomDrawerHeight
            --     - config.panelDimensions.bottomDrawerFooterHeight
            --     - config.panelDimensions.bottomDrawerHeaderHeight
            --     - 10
            100
    in
    { id = args.id
    , padding = { edges | top = 4, bottom = 4 }
    , height = shrink |> maximum maxHeight
    , fontSize = UI.scaleFont config.windowSize -1
    , fontColor = UI.white
    , bgColor = UI.lunarGreen
    , otherOuterAttrs = []
    , otherInnerAttrs = []
    , onLoseFocus = args.onLoseFocus
    , onChange = args.onChange
    , selected = args.selected
    , label = args.label
    }


defaultButtonConfig :
    WindowSize
    ->
        { args
            | id : String
            , width : Length
            , height : Length
            , onPress : Maybe msg
        }
    -> ButtonConfig msg
defaultButtonConfig size args =
    { id = args.id
    , width = args.width
    , height = args.height
    , padding = { edges | left = 6, right = 6 }
    , fontSize = UI.scaleFont size -1
    , fontColor = UI.white
    , bgColor = UI.lunarGreen
    , otherAttrs = []
    , onPress = args.onPress
    }


defaultSelectConfig :
    WindowSize
    ->
        { args
            | buttonConfig : ButtonConfig msg
            , labelConfig : LabelConfig msg
            , optionListConfig : OptionListConfig a msg
            , optionConfig : OptionConfig a msg
            , direction : SelectDirection
        }
    -> SelectConfig a msg
defaultSelectConfig size args =
    { buttonConfig = args.buttonConfig
    , labelConfig = args.labelConfig
    , optionListConfig = args.optionListConfig
    , optionConfig = args.optionConfig
    , otherAttrs = []
    , direction = args.direction
    , borderRadius = 3
    }



--
-- PUBLIC API
--


select : Palette.PaletteSet -> Palette.ColorSetSelector -> SelectConfig a msg -> Openness -> List a -> Element msg
select palette selector config openness options =
    let
        openFn =
            case config.direction of
                SelectDown ->
                    below

                SelectUp ->
                    above
    in
    el
        (case openness of
            Open ->
                [ openFn <| selectOptionList palette selector config options ] ++ config.otherAttrs

            Closed ->
                config.otherAttrs
        )
    <|
        selectButton palette selector config openness



--
-- PRIVATE HELPERS
--


selectOption : Palette.PaletteSet -> Palette.ColorSetSelector -> OptionConfig a msg -> a -> Input.Option a msg
selectOption palette selector optionConfig option =
    Input.optionWith option
        (\optState ->
            let
                stateAttrs =
                    case optState of
                        Input.Idle ->
                            optionConfig.idleAttrs

                        Input.Focused ->
                            optionConfig.focusedAttrs

                        Input.Selected ->
                            Palette.getColorAttributesActive Palette.Action selector palette
            in
            row
                ([ width optionConfig.width
                 , height optionConfig.height
                 , alignLeft
                 , paddingEach optionConfig.padding
                 --, mouseOver <| Palette.getColorAttributesHover Palette.Action selector palette
                 ]
                    ++ optionConfig.otherAttrs
                    ++ stateAttrs
                )
                [ text <| optionConfig.titleGetter option ]
        )


selectOptionList : Palette.PaletteSet -> Palette.ColorSetSelector -> SelectConfig a msg -> List a -> Element msg
selectOptionList palette selector { optionConfig, optionListConfig, borderRadius, direction } options =
    el
        ([ height optionListConfig.height
         , width fill
         , clip
         , Font.size optionListConfig.fontSize
        --  , Font.color optionListConfig.fontColor
        --  , Bg.color optionListConfig.bgColor
         , case direction of
            SelectDown ->
                Border.roundEach { corners | bottomLeft = borderRadius, bottomRight = borderRadius }

            SelectUp ->
                Border.roundEach { corners | topLeft = borderRadius, topRight = borderRadius }
         ]
            ++ optionListConfig.otherOuterAttrs
            ++ Palette.getColorAttributes Palette.Action selector palette
        )
    <|
        el [ scrollbarY, width fill ] <|
            Input.radio
                ([ UI.id optionListConfig.id
                 , paddingEach optionListConfig.padding
                 , width fill
                 , Events.onLoseFocus optionListConfig.onLoseFocus
                 ]
                    ++ optionListConfig.otherInnerAttrs
                )
                { label = Input.labelHidden optionListConfig.label
                , onChange = optionListConfig.onChange
                , selected = optionListConfig.selected
                , options = List.map (selectOption palette selector optionConfig) options
                }


selectLabel : LabelConfig msg -> Openness -> Element msg
selectLabel labelConfig openness =
    let
        theIcon =
            case openness of
                Open ->
                    labelConfig.openIcon

                Closed ->
                    labelConfig.closedIcon
    in
    row
        ([ width fill ] ++ labelConfig.otherAttrs)
        [ el [ alignLeft, width fill ] <| text labelConfig.label
        , case theIcon of
            Just icon ->
                el [ alignRight ] <| UI.centeredIcon icon

            Nothing ->
                none
        ]


selectButton : Palette.PaletteSet -> Palette.ColorSetSelector -> SelectConfig a msg -> Openness -> Element msg
selectButton palette selector { labelConfig, buttonConfig, borderRadius, direction } openness =
    Input.button
        ([ UI.id buttonConfig.id
         , width buttonConfig.width
         , height buttonConfig.height
         , paddingEach buttonConfig.padding
         , Font.size buttonConfig.fontSize
        --  , Font.color buttonConfig.fontColor
        --  , Bg.color buttonConfig.bgColor
         , case ( direction, openness ) of
            ( SelectDown, Open ) ->
                Border.roundEach { corners | topLeft = borderRadius, topRight = borderRadius }

            ( SelectUp, Open ) ->
                Border.roundEach { corners | bottomLeft = borderRadius, bottomRight = borderRadius }

            ( _, Closed ) ->
                Border.rounded borderRadius
         ]
        ++ buttonConfig.otherAttrs
        ++ Palette.getColorAttributes Palette.Action selector palette
        ++ Palette.getBorderAttributes 1 Palette.Action selector palette
        )
        { onPress = buttonConfig.onPress
        , label = selectLabel labelConfig openness
        }
