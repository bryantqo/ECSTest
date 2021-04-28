module PAM.UI.Search exposing(..)

import Element as El exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Events
import PAM.UI.Basic as UI exposing(edges)
import PAM.UI.Palette as Palette
import PAM.UI.StandardElements as SE
import Json.Decode as Decode exposing (Decoder, decodeValue, succeed, fail )

import Task

type alias Model data =
    { searchInput : String
    , suggestions : SuggestionsOrSearch data
    , hideSuggestions : Bool
    , placeholder : Maybe String
    , directionVisible : Bool
    }

type SuggestionsOrSearch data
    = None
    | Suggestions (List (Suggestion data))
    | Search (List (Suggestion data))

defaultModel : Model data
defaultModel =
    { searchInput = ""
    , suggestions = None
    , placeholder = Just "Enter search"
    , directionVisible = False
    , hideSuggestions = True
    }

getSearch : Model data -> String
getSearch =
    .searchInput

type alias Suggestion data =
    { displayName : String
    , data : data
    }


type Msg data
    = SearchClicked
    | Internal InternalMsg
    | SearchTextChanged String
    | SuggestionSelected data
    | ClickClearSearchButton
    | EnterWasPressed

type InternalMsg 
    = NoOp
    | SearchChanged String
    | DismissSuggestions


type alias API parentMsg data =
    { onInternalMessage : InternalMsg -> parentMsg
    , onSearchClicked : parentMsg
    , onSearchTextChanged : String -> parentMsg
    , onSuggestionSelected : data -> parentMsg
    }

type alias APITranslator parentMsg data =
    Msg data -> parentMsg


apiTranslator : API parentMsg data -> APITranslator parentMsg data
apiTranslator td msg =
    case msg of
        Internal m ->
            td.onInternalMessage m

        SearchClicked ->
            td.onSearchClicked

        SearchTextChanged txt ->
            td.onSearchTextChanged txt
        
        SuggestionSelected itm ->
            td.onSuggestionSelected itm

        ClickClearSearchButton ->
            td.onInternalMessage (SearchChanged "")
        
        EnterWasPressed ->
            td.onSearchClicked 


onEnter : msg -> El.Attribute msg
onEnter msg =
    El.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail ""
                    )
            )
        )


update : Model data -> InternalMsg -> ( Model data, Cmd (Msg data) )
update model msg =
    case msg of
        SearchChanged txt ->        
            ( { model 
                | searchInput = txt
                , directionVisible = False
                , hideSuggestions = txt == ""
                , suggestions = None
                }
            , Task.perform
                (always (SearchTextChanged txt))
                <| Task.succeed ""
            )
                 

        DismissSuggestions ->
            ( { model | hideSuggestions = True }
            , Cmd.none
            )
        
        _ ->
            ( model, Cmd.none )


setDirectionVisibility : Model data -> Model data
setDirectionVisibility model =
    let
        visibility = 
            case model.directionVisible of 
                False ->
                    True
                _ ->
                    False
                
    in
    { model | directionVisible = visibility
    }

setSuggestionResults : Model data -> List (Suggestion data) -> Model data
setSuggestionResults model suggestions =
    { model
    | suggestions = Suggestions suggestions 
    , hideSuggestions = False
    }

hideSuggestionView : Model data -> Model data
hideSuggestionView model =
    ( { model 
    | hideSuggestions = True 
    , suggestions = None
    }
    )

setSearchResults : Model data -> List (Suggestion data) -> Model data
setSearchResults model suggestions =
    { model
    | suggestions = Search suggestions 
    , hideSuggestions = False
    }

setSearch : Model data -> String -> Model data
setSearch model search =
    { model | searchInput = search
    }

suggestionView : Model data -> 
    { env
        | device : El.Device
        , windowSize : UI.WindowSize
    }
    -> ( Suggestion data -> Element (Msg data)) 
    -> Element (Msg data)
suggestionView model env sv =
    case model.suggestions of
        None ->
            none
        _ ->
            let
                suggestions =
                    case model.suggestions of
                        Suggestions s ->
                            s
                        Search s ->
                            s
                        _ ->
                            []
            in
            if not model.hideSuggestions then
                column 
                    [ height <| fill                 
                    , paddingEach { left = 0, right = 0, top = 2, bottom = 0 }
                    ]
                [ row
                    [ width fill 
                    , Bg.color <| UI.fadeColor 0.9 <| UI.black
                    , Font.color UI.white
                    , height <| px 30
                    , padding 5
                    ]
                    [ el [ centerY ]
                        <| text 
                        <| case model.suggestions of 
                            Suggestions _ ->
                                "Suggestions"
                            Search _ ->
                                "Search Results"
                            _ ->
                                ""
                    , el 
                        [ alignRight
                        ]
                        <| Input.button 
                            [ alignRight
                            ]
                            { onPress = Just (Internal DismissSuggestions)
                            , label = 
                                row
                                    [ width fill                        
                                    , Font.color UI.white ]
                                    [ UI.centeredIcon "fa fa-times"
                                    ]
                            }
                    ]
                , el
                            [ width <| maximum 550 <| shrink
                            , scrollbarY
                            , height <| maximum 400 <| shrink
                            , Bg.color <| UI.fadeColor 0.70 <| UI.black
                            , Font.color <| UI.white
                            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 6, bottomRight = 6 }
                            , padding 4
                            ]
                <| if List.length suggestions > 0 then
                        textColumn
                                [ width fill
                                , height fill
                                ]
                                <| List.map 
                                    ( \a -> 
                                        paragraph 
                                            [ width fill
                                            , Events.onClick <| SuggestionSelected a.data
                                            , Border.widthEach { edges | bottom = 1 }
                                            , Border.color <| UI.fadeColor 0.25 UI.white
                                            ]
                                            [sv a]
                                    ) suggestions
                        
                else
                    if String.length model.searchInput > 0 then
                        textColumn
                            [ width fill
                            , height fill
                            ]
                            [ paragraph 
                                [ width fill
                                , Border.widthEach { edges | bottom = 1 }
                                , Border.color <| UI.fadeColor 0.25 UI.white
                                , padding 10
                                ]
                                [ text "No results found."
                                ]
                            ]
                    else
                        none  
                ]  
            else
                none


    
clearIconView : Model data -> 
    { env
        | device : El.Device
        , windowSize : UI.WindowSize        
    }     
    -> Element (Msg data)

clearIconView model env =
    if String.length model.searchInput > 0 then
            row
                [height <| shrink
                , clip
                , centerY
                ]
                [  Input.button 
                    -- [ paddingEach { left = 10, right = 0, top = 0, bottom = 3 }
                    -- ]
                    [ paddingEach { edges | left = 10 }
                    ]
                    { onPress = Just ClickClearSearchButton
                    , label = 
                        row
                        [ width fill                        
                        , Font.color <| rgba 0 0 0 0.75 -- env.palette.action.tertiary.text
                        ]
                        [ UI.centeredIcon "fa fa-times"
                        ]
                    }
                ]
    else
        el[]
        (text "")

view : Model data -> 
    { env
        | device : El.Device
        , windowSize : UI.WindowSize
        , palette : Palette.PaletteSet
    } 
    -> ( Suggestion data -> Element (Msg data))
    -> Element (Msg data)
view model env sv =
    el 
        [ width <| minimum 300 <| fill
        , height fill
        , Font.color <| env.palette.action.tertiary.text
        , inFront <| clearIconView model env
        , inFront <|
            SE.button 
                SE.PrimaryAction 
                env 
                [ alignRight 
                , Border.rounded 16
                , width <| px 125
                ]
                { onPress = Just SearchClicked
                , label = 
                    [ text "Search" ]
                }
        , below <| suggestionView model env sv
        , Border.rounded 1
        ]
        <| el 
            [ width fill
            , paddingEach { edges | right = 2}
            ]
            <| row 
            [ width fill
            , height <| UI.inputHeight env
            , clip
            , Bg.color <| env.palette.action.tertiary.color
            , Border.rounded 14
            , Border.width 1
            , Border.color <| rgb 0.7 0.7 0.7
            , centerY
            , paddingEach { edges | right = 154 }
            ]
            [ Input.text 
                [ height <| shrink
                , clip
                , width fill
                , Bg.color <| UI.transparent0
                , Border.width 0
                , paddingEach 
                    { edges 
                    | left = 25
                    , top = 1
                    }
                , onEnter EnterWasPressed 
                ]
                { onChange = SearchChanged >> Internal
                , text = model.searchInput
                , placeholder = model.placeholder 
                    |> Maybe.map 
                        (\v -> Input.placeholder 
                            [ height <| shrink
                            , Bg.color <| UI.transparent0
                            , Border.width 0
                            , paddingEach 
                                { edges 
                                | left = 8
                                , top = 1
                                }
                            , centerY
                            , Font.size <| UI.scaleFont env.windowSize 1
                            , Font.color <| UI.fadeColor 0.5 env.palette.action.tertiary.text
                            ] 
                            (text v )) 
                , label = Input.labelHidden "Search"
                }
            ]