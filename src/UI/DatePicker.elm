module PAM.UI.DatePicker exposing
    ( Msg, DateEvent(..), InputError(..), DatePicker
    , init, initFromDate, initFromDates, update, view, isOpen, focusedDate, getInitialDate
    , Settings, defaultSettings, pick, between, moreOrLess, from, to, off, open, close
    , SelectedDate(..), SelectionMode(..), DateRange(..)
    , setPickedDate, Direction(..)
    )

{-| A customizable date picker component.


@docs Msg, DateEvent, InputError, DatePicker
@docs init, initFromDate, initFromDates, update, view, isOpen, focusedDate, getInitialDate


# Settings

@docs Settings, defaultSettings, pick, between, moreOrLess, from, to, off, open, close

-}

import Date exposing (Date, Month, day, month, year)
import PAM.UI.DatePicker.Date exposing (..)
import Html
import Html.Attributes as Attrs exposing (placeholder, selected, tabindex, type_, value)
--import Html.Events exposing (on, onBlur, onClick, onFocus, onInput, targetValue)
import Html.Keyed
import Json.Decode as Json
import Task
import Time exposing (Weekday(..))

import Regex



import Element as El exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Element.Events as Events
import PAM.UI.Basic as UI exposing (Openness(..), PanelDimensions, WindowSize, borders, corners, edges)
import PAM.UI.Palette as Palette


{-| An opaque type representing messages that are passed inside the DatePicker.
-}
type Msg
    = CurrentDate Date
    | ChangeFocus Date
    | Pick Date
    | Text String
    | SecondText String
    | SubmitText
    | Focus
    | Blur
    | OpenPicker
    | ClosePicker
    | MouseDown
    | MouseUp
    | GoToToday
    | Clear
    | NoOp


{-| The type of date picker settings.
-}
type alias Settings =
    { placeholder : String
    , classNamespace : String
    , containerClassList : List ( String, Bool )
    , inputClassList : List ( String, Bool )
    , inputName : Maybe String
    , inputId : String
    , inputAttributes : List (Html.Attribute Msg)
    , isDisabled : Bool
    , parser : String -> Result String Date
    , dateFormatter : Date -> String
    , dayFormatter : Weekday -> String
    , monthFormatter : Month -> String
    , yearFormatter : Int -> String
    , firstDayOfWeek : Weekday
    , changeYear : YearRange
    , selectionMode : SelectionMode
    , disableTextInput : Bool
    , openDirection : Direction
    }

type Direction 
    = Up
    | Down


type alias Model =
    { open : Bool
    , forceOpen : Bool
    , focused : Maybe Date -- date currently center-focused by picker, but not necessarily chosen
    , inputText : InputValidity
    , secondInputText : InputValidity
    , today : Date -- actual, current day as far as we know
    , selection : SelectedDate
    }

type InputValidity
    = NotEntered
    | InputInvalid String
    | InputValid String


{-| The DatePicker model. Opaque, hence no field docs.
-}
type DatePicker
    = DatePicker Model


type SelectionMode
    = SingleOnly
    | DateRange

type SelectedDate
    = NoneSelected
    | Single Date
    | Range DateRange


type DateRange
    = FullRange Date Date
    | Start Date
    | End Date

{-| A record of default settings for the date picker. Extend this if
you want to customize your date picker.

    import DatePicker exposing (defaultSettings)

    DatePicker.init { defaultSettings | placeholder = "Pick a date" }

NO LONGER VALID
To disable certain dates:

    import Date exposing (Day(..), dayOfWeek)
    import DatePicker exposing (defaultSettings)

    DatePicker.init { defaultSettings | isDisabled = \d -> dayOfWeek d `List.member` [ Sat, Sun ] }

-}
defaultSettings : Settings
defaultSettings =
    { placeholder = "Please pick a date..."
    , classNamespace = "elm-datepicker--"
    , containerClassList = []
    , inputClassList = []
    , inputName = Nothing
    , inputId = "Picker"
    , inputAttributes =
        [ Attrs.required False
        ]
    , isDisabled = False
    , parser = Date.fromIsoString
    , dateFormatter = sampleFormatter
    , dayFormatter = formatDay
    , monthFormatter = formatMonth
    , yearFormatter = String.fromInt
    , firstDayOfWeek = Sun
    , changeYear = off
    , selectionMode = SingleOnly
    , disableTextInput = False
    , openDirection = Down
    }


sampleFormatter : Date -> String
sampleFormatter day =
    day |> Date.format "yyyy-MM-dd"

yearRangeActive : YearRange -> Bool
yearRangeActive yearRange =
    yearRange /= Off


{-| Select a range of date to display

    DatePicker.init { defaultSettings | changeYear = between 1555 2018 }

-}
between : Int -> Int -> YearRange
between start end =
    if start > end then
        Between end start

    else
        Between start end


{-| Select a symmetric range of date to display

    DatePicker.init { defaultSettings | changeYear = moreOrLess 10 }

-}
moreOrLess : Int -> YearRange
moreOrLess range =
    MoreOrLess range


{-| Select a range from a given year to this year

    DatePicker.init { defaultSettings | changeYear = from 1995 }

-}
from : Int -> YearRange
from year =
    From year


{-| Select a range from this year to a given year

    DatePicker.init { defaultSettings | changeYear = to 2020 }

-}
to : Int -> YearRange
to year =
    To year


{-| Turn off the date range

    DatePicker.init { defaultSettings | changeYear = off }

-}
off : YearRange
off =
    Off



{-| The default initial state of the Datepicker. You must execute
the returned command (which, for the curious, sets the current date)
for the date picker to behave correctly.

    init =
        let
            ( datePicker, datePickerFx ) =
                DatePicker.init
        in
        ( { picker = datePicker }, Cmd.map ToDatePicker datePickerfx )

-}
init : ( DatePicker, Cmd Msg )
init =
    ( DatePicker <|
        { open = False
        , forceOpen = False
        , focused = Nothing
        , inputText = NotEntered
        , secondInputText = NotEntered
        , today = initDate
        , selection = NoneSelected
        }
    , Task.perform CurrentDate Date.today
    )


{-| Initialize a DatePicker with a given Date

    init date =
        ( { picker = DatePicker.initFromDate date }, Cmd.none )

-}
initFromDate : Date -> DatePicker
initFromDate date =
    DatePicker <|
        { open = False
        , forceOpen = False
        , focused = Just date
        , inputText = NotEntered
        , secondInputText = NotEntered
        , today = date
        , selection = NoneSelected
        }


{-| Initialize a DatePicker with a date for today and Maybe a date picked

    init today date =
        ( { picker = DatePicker.initFromDates today date }, Cmd.none )

-}
initFromDates : Date -> Maybe Date -> DatePicker
initFromDates today date =
    DatePicker <|
        { open = False
        , forceOpen = False
        , focused = date
        , inputText = NotEntered
        , secondInputText = NotEntered
        , today = today
        , selection = NoneSelected
        }


prepareDates : Date -> Weekday -> { currentMonth : Date, currentDates : List Date }
prepareDates date firstDayOfWeek =
    let
        weekdayAsInterval =
            weekdayToInterval firstDayOfWeek

        firstOfMonth =
            Date.fromCalendarDate (year date) (month date) 1

        -- First shown date
        -- If the first of a month is a sunday and firstDayOfWeek is sunday then its the first of the month
        -- Otherwise the daterange starts in the month before the current month
        start =
            Date.fromCalendarDate (year date) (month date) 1
                |> Date.floor weekdayAsInterval

        end =
            Date.add Date.Months 1 firstOfMonth
                |> Date.ceiling weekdayAsInterval
    in
    { currentMonth = date
    , currentDates = Date.range Date.Day 1 start end
    }


{-| Expose if the datepicker is open
-}
isOpen : DatePicker -> Bool
isOpen (DatePicker model) =
    model.open


{-| Expose the currently focused date
-}
focusedDate : DatePicker -> Maybe Date
focusedDate (DatePicker model) =
    model.focused


{-| Expose the initial date

When you initialize the DatePicker using function `init` the resulting `Cmd Msg` fetches todays date.
This date is then stored in the DatePicker's model as initial date.

In some scenarios, you want use todays date in combination with `DatePicker.Settings`.
This allows you to use todays date without storing it yourself.

Check the `simple-with-validate-date` example for an example usage.

-}
getInitialDate : DatePicker -> Date
getInitialDate (DatePicker model) =
    model.today


{-| The event resulting from a `DatePicker.update`.
Three things can happen:

  - Nothing
  - The user might pick a date through clicking or typing
  - Or the user typed a date that is either invalid or disabled

If you do not care about case error handling for invalid inputs, just matching on `Picked` should suffice.

Have a look at the `nightwash-simple` example for basic error handling with `InputError`.

-}
type DateEvent
    = None
    | FailedInput InputError
    | Picked (Maybe SelectedDate)
    | PickerOpened
    | ControlInteraction


{-| When typing a date it can go wrong in two ways:

  - `Invalid String`: the typed string cannot be parsed into a date. The error string is given be the parser defined in the settings.
  - `Disabled Date`: a valid date was typed, but it is disabled as defined in the settings.

-}
type InputError
    = Invalid String
    | EmptyString
    | Disabled Date


normalizeRange : Date -> Date -> SelectedDate
normalizeRange start date =
    case Date.compare date start of
        GT ->
            (Range (FullRange start date))

        LT ->
            (Range (FullRange date start))

        EQ ->
            (Range (FullRange start date))


normalizeRangeMaybe : Date -> Date -> Maybe SelectedDate
normalizeRangeMaybe start date =
    case Date.compare date start of
        GT ->
            (Range (FullRange start date)) |> Just

        LT ->
            Nothing

        EQ ->
            (Range (FullRange start date)) |> Just

updateText : Settings -> DatePicker -> ( DatePicker, DateEvent )
updateText settings (DatePicker ( { focused } as model )) =
    let
        startParse = 
            case model.inputText of
                InputValid text ->
                    text 
                    |> settings.parser
                    |> Just
                _ ->
                    Nothing
                


            -- |> Maybe.andThen 
            --     (\txt ->
            --         if txt == "" then
            --             Nothing
            --         else
            --             Just txt
            --     )
            -- |> Maybe.map settings.parser 
            

        endParse = 
            case model.secondInputText of
                InputValid text ->
                    text 
                    |> settings.parser
                    |> Just
                _ ->
                    Nothing
        
        foc2 = 
            case settings.selectionMode of
                SingleOnly ->
                    startParse
                    |> Maybe.map 
                        (\date ->
                            case date of
                                Ok d ->
                                    Single d
                                Err _ ->
                                    NoneSelected
                        )

                DateRange ->
                    case (startParse, endParse) of


                        (Just (Ok newStart), Just (Ok newEnd)) ->
                            normalizeRangeMaybe newStart newEnd
                        (Just (Ok newStart), Nothing) ->
                            Range (Start newStart) |> Just
                        (Nothing, Just (Ok newEnd)) ->
                            Range (End newEnd) |> Just

                            
                        (Nothing, Nothing) ->
                            NoneSelected |> Just


                        (Just (Err _), _) ->
                            Nothing
                        (_, Just (Err _)) ->
                            Nothing
        inputValid =
            case (settings.selectionMode, model.inputText, model.secondInputText) of
                (SingleOnly, InputInvalid val, _) ->
                    FailedInput ( Invalid ( val ++ " is not a valid date in the mm/dd/yyyy format") )
                (SingleOnly, InputValid val, _) ->
                    foc2 |> Picked
                (_,_,_) ->
                    None

    in
    ( DatePicker 
        { model 
        | selection = foc2 |> Maybe.withDefault model.selection
        }
    , inputValid
    )




setTextInputs : Settings -> DatePicker -> DatePicker
setTextInputs settings (DatePicker model) =
    { model
    | inputText = 
        case model.selection of
            Single d ->
                settings.dateFormatter d |> InputValid
            Range (Start d) ->
                settings.dateFormatter d |> InputValid
            Range (FullRange d _) ->
                settings.dateFormatter d |> InputValid
            _ ->
                NotEntered
    , secondInputText = 
        case model.selection of
            Range (End d) ->
                settings.dateFormatter d |> InputValid
            Range (FullRange _ d) ->
                settings.dateFormatter d |> InputValid
            _ ->
                NotEntered
    }
    |> DatePicker

{-| The date picker update function. The second tuple member represents a user action to change the
date.
-}
update : Settings -> Msg -> DatePicker -> ( DatePicker, DateEvent )
update settings msg (DatePicker ({ forceOpen, focused } as model)) =
    case msg of
        Clear ->
            ( DatePicker <|
                { model
                | inputText = NotEntered
                , secondInputText = NotEntered
                , selection = NoneSelected
                }
            , Picked (Just NoneSelected)
            )
            -- updateText settings 
            --     ( DatePicker 
            --         { model 
            --         | inputText = NotEntered 
            --         , secondInputText = NotEntered
            --         , selection = NoneSelected
            --         } 
            --     )
        CurrentDate date ->
            ( DatePicker { model | focused = Nothing, today = date }, None )

        ChangeFocus date ->
            ( DatePicker { model | focused = Just date }, ControlInteraction )

        GoToToday ->
            ( DatePicker { model | focused = Just model.today }, ControlInteraction )

        Pick date ->
            case settings.selectionMode of
                SingleOnly ->
                    ( DatePicker <|
                        { model
                        | inputText = NotEntered
                        , secondInputText = NotEntered
                        , selection = (Single date)
                        , open = False
                        }
                    , Picked (Just (Single date))
                    )

                DateRange -> 
                    let
                        newDate = 
                            case model.selection of
                                (Range (Start start)) ->
                                    normalizeRange date start 
                                (Range (End end)) ->
                                    normalizeRange end date 
                                _ ->
                                    (Range (Start date))
                    in
                    (
                        { model
                        | selection = newDate
                        }
                        |> DatePicker
                        |> setTextInputs settings
                    , Picked <| Just newDate
                    )

        Text text ->
            if text /= "" then
                let
                    dateFormatRegex = Regex.fromString  "^\\d{1,2}/\\d{1,2}/\\d{4}$" |> Maybe.withDefault Regex.never

                    splitRegex = Regex.fromString "/" |> Maybe.withDefault Regex.never
                    parts = Regex.split splitRegex text

                    monthRegex = 
                        Regex.fromString  "^[1-9]$|^0[1-9]$|^1[1-2]$" |> Maybe.withDefault Regex.never

                    dayRegex =
                        Regex.fromString  "^[1-9]$|^0[1-9]$|^[1-2][0-9]$|^3[0-1]$" |> Maybe.withDefault Regex.never

                    yearRegex = 
                        Regex.fromString  "^[1-2][0-9]{3}$" |> Maybe.withDefault Regex.never

                    monthMatches =
                        parts
                        |> List.head
                        |> Maybe.map (Regex.contains monthRegex)
                        |> Maybe.withDefault False
                    dayMatches =
                        parts
                        |> List.drop 1
                        |> List.head
                        |> Maybe.map (Regex.contains dayRegex)
                        |> Maybe.withDefault False
                    yearMatches =
                        parts
                        |> List.drop 2
                        |> List.head
                        |> Maybe.map (Regex.contains yearRegex)
                        |> Maybe.withDefault False

                    validText = 
                            monthMatches &&
                            dayMatches &&
                            yearMatches

                    inputText =
                        if validText then
                            InputValid text
                        else
                            InputInvalid text

                    --If validText then parse the date

                in
                updateText settings ( DatePicker { model | inputText = inputText } )
            else
                updateText settings ( DatePicker { model | inputText = NotEntered } )

        SecondText text ->
            if text /= "" then
                let
                    dateFormatRegex = Regex.fromString  "^\\d{1,2}/\\d{1,2}/\\d{4}$" |> Maybe.withDefault Regex.never

                    splitRegex = Regex.fromString "/" |> Maybe.withDefault Regex.never
                    parts = Regex.split splitRegex text

                    monthRegex = 
                        Regex.fromString  "^[1-9]$|^0[1-9]$|^1[1-2]$" |> Maybe.withDefault Regex.never

                    dayRegex =
                        Regex.fromString  "^[1-9]$|^0[1-9]$|^[1-2][0-9]$|^3[0-1]$" |> Maybe.withDefault Regex.never

                    yearRegex = 
                        Regex.fromString  "^[1-2][0-9]{3}$" |> Maybe.withDefault Regex.never

                    monthMatches =
                        parts
                        |> List.head
                        |> Maybe.map (Regex.contains monthRegex)
                        |> Maybe.withDefault False
                    dayMatches =
                        parts
                        |> List.drop 1
                        |> List.head
                        |> Maybe.map (Regex.contains dayRegex)
                        |> Maybe.withDefault False
                    yearMatches =
                        parts
                        |> List.drop 2
                        |> List.head
                        |> Maybe.map (Regex.contains yearRegex)
                        |> Maybe.withDefault False

                    validText = 
                            monthMatches &&
                            dayMatches &&
                            yearMatches

                    inputText =
                        if validText then
                            InputValid text
                        else
                            InputInvalid text

                    --If validText then parse the date

                in
                updateText settings ( DatePicker { model | secondInputText = inputText } )
            else
                updateText settings ( DatePicker { model | secondInputText = NotEntered } )

        SubmitText ->
            ( DatePicker model, None)

        Focus ->
            ( DatePicker model, None)

        Blur ->
            ( DatePicker { model | open = False }, None)

        OpenPicker ->
            ( 
                { model 
                | open = True
                , focused = Nothing 
                }
                |> DatePicker
                |> setTextInputs settings
            , PickerOpened
            )

        ClosePicker ->
            ( DatePicker { model | open = False }, None)

        MouseDown ->
            ( DatePicker { model | forceOpen = True }, None )

        MouseUp ->
            ( DatePicker { model | forceOpen = False }, None )

        NoOp ->
            ( DatePicker model , None )


{-| Generate a message that will act as if the user has chosen a certain date,
so you can call `update` on the model yourself.
Note that this is different from just changing the "current chosen" date,
since the picker doesn't actually have internal state for that.
Rather, it will:

  - change the calendar focus

  - replace the input text with the new value

  - close the picker

    update datepickerSettings (pick someDate) datepicker

-}
pick : Date -> Msg
pick =
    Pick


{-| Generate a message that will act as if the user has focused on the input element.
This will open the datePicker

    update datepickerSettings open datepicker

Example usage is demonstrated in the `simple-nightwash`-example.

-}
open : Msg
open =
    Focus


{-| Generate a message that will act as if the user has removed focus from the input element.
This will close the datePicker

    update datepickerSettings close datepicker

Example usage is demonstrated in `simple-nightwash`-example.

-}
close : Msg
close =
    Blur


setPickedDate : Settings -> SelectedDate -> DatePicker -> DatePicker
setPickedDate settings pickedDate (DatePicker (model)) =
    ( 
        { model
        | selection = pickedDate
        }
        |> DatePicker
        |> setTextInputs settings
    )

{-| The date picker view. The Date passed is whatever date it should treat as selected.
-}
view : Settings -> DatePicker -> Element Msg
view settings (DatePicker (model as datepicker)) =
    let
        pickedDate = 
            case model.selection of
                NoneSelected -> Nothing
                _ -> Just model.selection

    in
    wrappedRow
        [ width fill
        , spaceEvenly
        , UI.id settings.inputId
        , ( case settings.openDirection of
            Up ->
                above
            Down ->
                below
            )
            <| if model.open then
                el 
                    [ padding 3     
                    , Font.color UI.black
                    , alignRight
                    , UI.id (settings.inputId ++ "-picker")
                    ]
                <| column
                    (UI.modalShadow [ width <| px 175
                    , height shrink
                    , Bg.color UI.white
                    , UI.style "z-index" "100"
                    , Border.rounded 3
                    , Border.color <| UI.darkenColor 0.9 UI.white
                    , Border.width 1
                    , Border.solid
                    , clip
                    ])
                    [ yearMonthDisplay pickedDate settings model
                    , calendarDisplay pickedDate settings model
                    , closeTodayControls
                    ]

            else
                el 
                    [ UI.id  (settings.inputId ++ "-picker") 
                    ]
                    none
        , Events.onLoseFocus close
        ]
    ( [ if settings.isDisabled || settings.disableTextInput then
            el 
                [ width <| shrink --minimum 84
                , height <| px 25 --TODO this needs to pull from UI.inputHeight which requires a device and windowsize!
                , Border.rounded 4
                , Font.color UI.doveGray
                , Bg.color <| UI.darkenColor 0.05 UI.white
                , width fill
                ]
            <| el
                [ height shrink
                , width shrink
                , centerY
                ]
            <| text 
            <| ( pickedDate 
                    |> Maybe.map 
                    (\val ->
                        case val of
                            Single date ->
                                settings.dateFormatter date
                            Range (Start start) ->
                                settings.dateFormatter start
                            Range (FullRange start _) ->
                                settings.dateFormatter start
                            _ ->
                                ""
                    )
                    |> Maybe.withDefault ""
                )
            else if settings.disableTextInput then
                el 
                [ width <| shrink
                , height <| px 25
                , Border.rounded 4
                ]
                <| el
                    [ height shrink
                    , width shrink
                    , centerY
                    ]
                <| text 
                <| ( pickedDate 
                        |> Maybe.map 
                        (\val ->
                            case val of
                                Single date ->
                                    settings.dateFormatter date
                                Range (Start start) ->
                                    settings.dateFormatter start
                                Range (FullRange start _) ->
                                    settings.dateFormatter start
                                _ ->
                                    "mm/dd/yyyy"
                        )
                        |> Maybe.withDefault "mm/dd/yyyy"
                    )
            else
                el 
                ( [ width fill
                , padding 0
                , height fill
                ] 
                )
                <| Input.text
                ( [ width fill --<| px 84
                , height <| px 35 --TODO: Use UI.inputHeight
                , Border.rounded 4
                , paddingXY 5 12
                , Font.color UI.black
                , inFront
                    <| Input.button
                        [ width <| px 20
                        , height <| px 20
                        , alignRight
                        , centerY
                        , Font.color <| rgba 0 0 0 0.25
                        ]
                        { label = el [ width shrink, height shrink, centerX, centerY ] <| UI.centeredIcon "fa fa-times"
                        , onPress = Just <| Clear
                        }

                ] ++
                    case model.inputText of --TODO: Remove this
                        InputInvalid _ ->
                            [ Border.color <| El.rgb255 255 0 0
                            ]
                        _ ->
                            [ Border.width 1
                            ]
                )


                { onChange = 
                    Text
                , text =
                    case model.inputText of
                        InputValid text ->
                            text
                        InputInvalid text ->
                            text
                        NotEntered ->
                            pickedDate 
                            |> Maybe.map 
                            (\val ->
                                case val of
                                    Single date ->
                                        settings.dateFormatter date
                                    Range (Start start) ->
                                        settings.dateFormatter start
                                    Range (FullRange start _) ->
                                        settings.dateFormatter start
                                    _ ->
                                        ""
                            )
                            |> Maybe.withDefault ""
                            
                , placeholder =
                    Just <|
                        Input.placeholder
                        [ paddingXY 3 0
                        , height shrink
                        , centerY
                        ] <|
                            el [] <|
                                El.text "mm/dd/yyyy"
                , label = Input.labelHidden "Start Date"
                }
    ]
    ++
    ( case settings.selectionMode of
        DateRange ->
            [ el
                [ centerY
                , width shrink
                , paddingXY 5 0
                ]
            <| El.text "to"
            , if settings.isDisabled || settings.disableTextInput then
                el 
                    [ width <| shrink --minimum 84
                    , height <| px 25
                    , Border.rounded 4
                    , Bg.color <| UI.darkenColor 0.05 UI.white
                    , width fill
                    , padding 5
                    , Font.color UI.doveGray
                    ]
                <| el
                    [ height shrink
                    , width shrink
                    , centerY
                    ]
                <| text 
                <| ( pickedDate 
                            |> Maybe.map 
                            (\val ->
                                case val of
                                    Single date ->
                                        ""
                                    Range (End end) ->
                                        settings.dateFormatter end
                                    Range (FullRange _ end) ->
                                        settings.dateFormatter end
                                    _ ->
                                        ""
                            )
                            |> Maybe.withDefault ""
                        )
            else if settings.disableTextInput then
                el 
                [ width <| shrink
                , height <| px 25
                , Border.rounded 4
                , padding 5
                ]
                <| el
                    [ height shrink
                    , width shrink
                    , centerY
                    ]
                <| text 
                <| ( pickedDate 
                        |> Maybe.map 
                        (\val ->
                            case val of
                                Single date ->
                                    settings.dateFormatter date
                                Range (End end) ->
                                    settings.dateFormatter end
                                Range (FullRange _ end) ->
                                    settings.dateFormatter end
                                _ ->
                                    "mm/dd/yyyy"
                        )
                        |> Maybe.withDefault "mm/dd/yyyy"
                    )
            else
                el 
                ( [ width fill
                , height fill
                , padding 2
                ] ++
                    case model.secondInputText of --TODO: Remove this
                        InputInvalid _ ->
                            [ Border.color <| El.rgb255 255 0 0
                            , Border.width 1
                            , Border.rounded 4
                            ]
                        _ ->
                            [ Border.color <| UI.transparent0
                            , Border.width 1
                            , Border.rounded 4
                            ]
                )
                <| Input.text
                     [ width fill --<| px 84
                    , height <| px 20
                    , Border.rounded 4
                    , paddingXY 5 12
                    , Font.color UI.black
                    ]
                    
                    { onChange = 
                        SecondText
                    , text =
                        case model.secondInputText of
                        InputValid text ->
                            text
                        InputInvalid text ->
                            text
                        NotEntered ->
                            pickedDate 
                            |> Maybe.map 
                            (\val ->
                                case val of
                                    Range (End end) ->
                                        settings.dateFormatter end
                                    Range (FullRange _ end) ->
                                        settings.dateFormatter end
                                    _ ->
                                        ""
                            )
                            |> Maybe.withDefault ""
                            
                    , placeholder =
                        Just <|
                            Input.placeholder
                            [ paddingXY 3 0
                            , height shrink
                            , centerY
                            --, Font.color <| UI.fadeColor 0.5 palette.action.tertiary.text
                            ] <|
                                el [] <|
                                    El.text "mm/dd/yyyy"
                    , label = Input.labelHidden "End Date"
                    }
            ]
        _ ->
            []
    )
    ++
    [ Input.button
            [ UI.title "Pick Date"
            , height fill
            , width shrink
            , paddingXY 5 0
            , Font.color UI.black
            ]
            { onPress = 
                Just 
                ( if not model.open then
                    OpenPicker
                else
                    ClosePicker
                )
            , label =
                el
                    [ width fill
                    ]
                <|
                    UI.centeredIcon "fa fa-calendar"
            }
    ]
    )


closeTodayControls : Element Msg
closeTodayControls =
    row 
        [ width fill
        , height shrink
        ]
        [ Input.button
            [ UI.title "Close"
            , height shrink
            , width fill
            , centerX
            , Border.widthEach { edges | right = 1 }
            , Bg.color <| UI.darkenColor 0.05 UI.white
            ]
            { onPress = Just ClosePicker
            , label =
                el
                    [ centerX
                    , centerY
                    , padding 2
                    ]
                <|
                    El.text "Close"
            }
        , Input.button
            [ UI.title "Go to today"
            , height shrink
            , width fill
            , centerX
            , Bg.color <| UI.darkenColor 0.05 UI.white
            ]
            { onPress = Just GoToToday
            , label =
                el
                    [ centerX
                    , centerY
                    , padding 2
                    ]
                <|
                    El.text "Today"
            }
        ]

getPriorityDay : Maybe Date -> Maybe SelectedDate -> SelectedDate -> Date -> SelectionMode -> Date
getPriorityDay focused pickedDate selection today mode =
    case mode of
        SingleOnly ->
            case (focused, pickedDate) of
                (Just day, _) ->
                    day
                (_, Just (Single day)) ->
                    day
                (_,_) ->
                    today
        DateRange ->
            case (focused, selection, pickedDate) of
                (Just day, _, _) ->
                    day
                (_, Range (Start start),_) ->
                    start
                (_, Range (FullRange start end),_) ->
                    start
                (_, Range (End end), _) ->
                    end
                (_, _, Just (Range (Start start))) ->
                    start
                (_, _, Just (Range (End end))) ->
                    end
                (_, _, Just (Range (FullRange start end))) ->
                    start
                (_, _,_) ->
                    today

yearMonthDisplay : Maybe SelectedDate -> Settings -> Model -> Element Msg
yearMonthDisplay pickedDate settings ({focused, today} as model) =
    column
        [ width fill
        , height shrink
        , padding 3
        , spacing 1
        ]
        [ monthDisplay pickedDate settings model
        , yearDisplay pickedDate settings model
        ]

monthDisplay : Maybe SelectedDate -> Settings -> Model -> Element Msg
monthDisplay pickedDate settings ({ focused, today } as model) =
    let

        currentDate =
            getPriorityDay focused pickedDate model.selection today settings.selectionMode
            

        { currentMonth, currentDates } =
            prepareDates currentDate settings.firstDayOfWeek
    in
    row
        [ width fill
        , height shrink
        , spaceEvenly
        , centerX
        , paddingXY 3 0
        ]
        [ Input.button
            [ UI.title "Previous Month"
            , height fill
            , width <| px 20
            ]
            { onPress = Just (ChangeFocus (Date.add Date.Months -1 currentDate))
            , label =
                el
                    [ width shrink
                    , alignLeft
                    ]
                <|
                    UI.icon "fa fa-chevron-left"
            }
        , El.text 
            <| settings.monthFormatter 
            <| month currentMonth
        , Input.button
            [ UI.title "Next Month"
            , height fill
            , width <| px 20
            ]
            { onPress = Just (ChangeFocus (Date.add Date.Months 1 currentDate))
            , label =
                el
                    [ width shrink
                    , alignRight
                    ]
                <|
                    UI.icon "fa fa-chevron-right"
            }
        ]

yearDisplay : Maybe SelectedDate -> Settings -> Model -> Element Msg
yearDisplay pickedDate settings ({ focused, today } as model) =
    let
        currentDate =
            getPriorityDay focused pickedDate model.selection today settings.selectionMode

        { currentMonth, currentDates } =
            prepareDates currentDate settings.firstDayOfWeek
    in
   row
        [ width shrink
        , height shrink
        , Border.color <| UI.darkenColor 0.25 UI.white
        , spacing 15
        , centerX
        , centerY
        ]
        [ Input.button
            [ UI.title "Previous Year"
            , height shrink
            , width shrink
            ]
            { onPress = Just (ChangeFocus (Date.add Date.Years -1 currentDate))
            , label =
                UI.icon "fa fa-chevron-left"
            }
        , el 
            [ width shrink
            , height shrink
            ]
            <| El.text 
            <| settings.yearFormatter 
            <| year currentMonth
        , Input.button
            [ UI.title "Next Year"
            , height shrink
            , width shrink
            ]
            { onPress = Just (ChangeFocus (Date.add Date.Years 1 currentDate))
            , label =
                UI.icon "fa fa-chevron-right"
            }
        ]

calendarDisplay : Maybe SelectedDate -> Settings -> Model -> Element Msg
calendarDisplay pickedDate settings ({ focused, today } as model) =
    let
        currentDate =
            getPriorityDay focused pickedDate model.selection today settings.selectionMode
            

        { currentMonth, currentDates } =
            prepareDates currentDate settings.firstDayOfWeek

        
        picked d =
            case pickedDate of
                Just (Single day) ->
                    (Date.toRataDie d == Date.toRataDie day)
                Just (Range (Start start)) ->
                    (Date.toRataDie d >= Date.toRataDie start)
                Just (Range (End end)) ->
                    (Date.toRataDie d <= Date.toRataDie end)
                Just (Range (FullRange start end)) ->
                    (Date.toRataDie d >= Date.toRataDie start) &&
                    (Date.toRataDie d <= Date.toRataDie end)
                _ ->
                    False


        isStart d =
            case pickedDate of
                Just (Single day) ->
                    (Date.toRataDie d == Date.toRataDie day)
                Just (Range (Start start)) ->
                    (Date.toRataDie d == Date.toRataDie start)
                Just (Range (FullRange start end)) ->
                    (Date.toRataDie d == Date.toRataDie start)
                _ ->
                    False

        isEnd d =
            case pickedDate of
                Just (Single day) ->
                    (Date.toRataDie d == Date.toRataDie day)
                Just (Range (End end)) ->
                    (Date.toRataDie d == Date.toRataDie end)
                Just (Range (FullRange start end)) ->
                    (Date.toRataDie d == Date.toRataDie end)
                _ ->
                    False

        isToday d =
            Date.toRataDie d == Date.toRataDie today

        isOtherMonth d =
            month currentDate /= month d

        weekList =
            groupDates currentDates
                |> List.map
                    (\rowDays ->
                        row 
                            [ width fill
                            , height shrink
                            , spaceEvenly
                            ]
                            (List.map (viewDayEl settings picked isOtherMonth isToday isStart isEnd) rowDays)
                    )
    in
    column
        [ width fill
        , height fill
        , Border.solid
        , Border.widthEach { borders | top = 1, bottom = 1 }
        , Border.color <| UI.darkenColor 0.5 UI.white
        ]
        [ row 
            [ width fill
            , Bg.color <| UI.darkenColor 0.05 UI.white
            , Border.solid
            , Border.widthEach { borders | bottom = 1 }
            , Border.color <| UI.darkenColor 0.3 UI.white
            , spaceEvenly
            ]
            [ el [ calendarDayWidth, calendarDayHeight ] <| el [ centerX, centerY ] <| El.text "S" 
            , el [ calendarDayWidth, calendarDayHeight ] <| el [ centerX, centerY ] <| El.text "M" 
            , el [ calendarDayWidth, calendarDayHeight ] <| el [ centerX, centerY ] <| El.text "T" 
            , el [ calendarDayWidth, calendarDayHeight ] <| el [ centerX, centerY ] <| El.text "W" 
            , el [ calendarDayWidth, calendarDayHeight ] <| el [ centerX, centerY ] <| El.text "T" 
            , el [ calendarDayWidth, calendarDayHeight ] <| el [ centerX, centerY ] <| El.text "F" 
            , el [ calendarDayWidth, calendarDayHeight ] <| el [ centerX, centerY ] <| El.text "S" 
            ]
        , column
            [ width fill
            , height fill
            ]
            weekList
        ]

calendarDayWidth : Attribute msg
calendarDayWidth =
    width fill

calendarDayHeight : Attribute msg
calendarDayHeight =
    height <| px 18

viewDayEl : Settings -> (Date -> Bool) -> (Date -> Bool) -> (Date -> Bool) -> (Date -> Bool) -> (Date -> Bool) -> Date -> Element Msg
viewDayEl settings picked isOtherMonth isToday isStart isEnd d =
    el
        [ calendarDayWidth
        , calendarDayHeight 
        , Border.width 1
        
        , if isToday d then
                Border.rounded 3
            else if isStart d then
                Border.roundEach 
                    { topLeft = 3
                    , bottomLeft = 3
                    , topRight = 0
                    , bottomRight = 0
                    }
            else if isEnd d then
                Border.roundEach 
                    { topLeft = 0
                    , bottomLeft = 0
                    , topRight = 3
                    , bottomRight = 3
                    }
            else
                Border.rounded 0
        , Border.width
            <| if isToday d then
                1
            else 0
        , Bg.color
                <| case ( picked d, isOtherMonth d) of
                    ( True, _ ) ->
                        UI.fadeColor 
                            ( if isStart d || isEnd d  then
                                0.5 
                            else
                                0.3
                            ) UI.azureRadiance
                    ( False, True ) ->
                        UI.darkenColor 0.1 UI.white
                    ( _ , _ ) ->
                        UI.white
        ]
        <| if settings.isDisabled then
            el
                [ width fill
                , height fill
                ]
            <| el
                    [ centerX
                    , centerY
                    , Font.color UI.doveGray
                    , padding 1
                    , if isToday d then
                        Font.bold
                    else
                        Font.regular
                    ]
                <| El.text 
                <|  String.fromInt <| Date.day d
        else
            Input.button
            [ width fill
            , height fill
            ]
            { onPress = 
                Just <| Pick d
            , label =
                el
                    [ centerX
                    , centerY
                    , if isToday d then
                        Font.bold
                    else
                        Font.regular
                    ]
                <| El.text 
                <|  String.fromInt <| Date.day d 
            }





{-| Turn a list of dates into a list of date rows with 7 columns per
row representing each day of the week.
-}
groupDates : List Date -> List (List Date)
groupDates dates =
    let
        go i xs racc acc =
            case xs of
                [] ->
                    List.reverse acc

                x :: xxs ->
                    if i == 6 then
                        go 0 xxs [] (List.reverse (x :: racc) :: acc)

                    else
                        go (i + 1) xxs (x :: racc) acc
    in
    go 0 dates [] []


maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr lhs rhs =
    case rhs of
        Just _ ->
            rhs

        Nothing ->
            lhs
