port module PAM.UI.LeftSidebarSearch exposing(..)

import Animation
import Element as El exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import RemoteData exposing (RemoteData, WebData, isSuccess)

import Http
import Browser
import Browser.Dom as Dom
import Task
import List
import List.Extra as LEx
import Maybe.Extra as MEx
import Html exposing (Html)
import Html.Attributes exposing (id)
import Html.Events
import PAM.UI.Basic as UI exposing (Openness(..), PanelDimensions, WindowSize, corners, edges)
import Json.Decode as Decode exposing (Decoder, decodeValue, succeed, fail )
import Json.Encode as E exposing (encode)
import PAM.Settings exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DecodeEx
import Json.Decode.Pipeline exposing (required, requiredAt)
import PlugMap.Plugins.Search as MapSearch


port moveMapCenter : (Float,Float) -> Cmd msg


type alias Model =
    { queryUrl : String
    , leftSearchInput : Maybe String
    , queriedFeature : WebData QueryedFeatures
    , errorMessage : String
    }


type Msg
    = Internal InternalMsg
    | ClickGoButton
    | ClickViewObjectDetailsNew Int
    | EnterWasPressed


type InternalMsg 
    = NoOp
    | LeftSidebarSearchChanged String
    | ClickGoButtonClicked 
    | QueryResponse (WebData QueryedFeatures)


type alias API parentMsg =
    { onInternalMessage : InternalMsg -> parentMsg
    , onClickViewObjectDetailsNew  : Int -> parentMsg
    }


type alias APITranslator parentMsg =
    Msg -> parentMsg


type alias Url =
    String


type alias Attributes =
    { id : Int
    }


type alias QueryedFeature =
    { id : Int
    , location : MapSearch.Location
    }   


type alias QueryedFeatures =
    List QueryedFeature


defaultQueryedFeature : QueryedFeature 
defaultQueryedFeature =
    { id = -1
    , location = MapSearch.defaultSearchLocation
    }   


defaultQueryedFeatures : QueryedFeatures 
defaultQueryedFeatures =
    [defaultQueryedFeature] 


decodeid : Decoder Int
decodeid =
    Decode.int


-- decodeAttributes : Decoder Attributes
-- decodeAttributes =
--     Decode.succeed Attributes
--         |> required "id" decodeid


queryedFeatureDecoder : Decoder QueryedFeature
queryedFeatureDecoder =
    Decode.succeed QueryedFeature        
        |> requiredAt ["attributes", "id"] decodeid
        |> required "geometry" MapSearch.decodeLocation

queryedFeaturesDecoder : Decoder QueryedFeatures
queryedFeaturesDecoder =
    Decode.at [ "features" ] (Decode.list queryedFeatureDecoder) |> DecodeEx.withDefault []



apiTranslator : API parentMsg -> APITranslator parentMsg
apiTranslator td msg =
    case msg of
        Internal m ->
            td.onInternalMessage m
        ClickGoButton ->
            td.onInternalMessage ClickGoButtonClicked 
        ClickViewObjectDetailsNew id ->            
            td.onClickViewObjectDetailsNew id
        EnterWasPressed ->
            td.onInternalMessage ClickGoButtonClicked 


getQueryResult :  Model -> Cmd Msg
getQueryResult model =
    let
        baseUrl = model.queryUrl
        queryTxt = 
            case model.leftSearchInput of 
                Just txt ->
                    String.toLower txt |> String.trim
                _ ->
                    ""
        queryString = 
            baseUrl ++ "?where=Lower(uniquefireidentifier)='" ++ queryTxt ++ "'+OR+Lower(name)='" ++ queryTxt ++ "'+OR+Lower(irwinid)='" ++ queryTxt ++ "'+&outFields=id&f=json"
            
        get : Http.Body -> Cmd Msg
        get body =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Accept" "application/json, text/javascript, */*; q=0.01" ]
                , url = queryString
                , body = body
                , expect = Http.expectJson (RemoteData.fromResult >> (QueryResponse >> Internal)) queryedFeaturesDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
        get Http.emptyBody


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

update : Model -> InternalMsg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        LeftSidebarSearchChanged txt ->            
            if String.length txt > 0 then
                ( { model 
                    | leftSearchInput = Just txt
                }
                , Cmd.none )                
            else 
                ( { model 
                    | leftSearchInput = Just txt
                    }
                , Cmd.none )
        ClickGoButtonClicked ->
            ( model
            , Cmd.batch
                [ getQueryResult model
                ] )        
        QueryResponse response ->
            let
                responseList = 
                    case response of
                        RemoteData.Success featureList ->
                            featureList
                        _ ->
                            []
                selectedFeature = 
                    LEx.last responseList
                sf = 
                    case selectedFeature of 
                        Just feature ->
                            feature
                        _ ->
                            defaultQueryedFeature
            in
                if sf.id > 0 then
                ( { model | queriedFeature = response,  errorMessage = "" }
                , Cmd.batch 
                            [ Task.perform
                                (always (ClickViewObjectDetailsNew sf.id))
                                <| Task.succeed ""
                            , moveMapCenter (sf.location.x, sf.location.y)
                            ]
                )
                else 
                ( {model | errorMessage = "No results found."}, Cmd.none )
        
        _ ->
            ( model, Cmd.none )

view: 
    { env
        | device : El.Device
        , windowSize : UI.WindowSize
        , panelDimensions : PanelDimensions
        , appSettings : AppSettings        
    } 
    -> Model
    -> Element Msg
view env model =
    let
        defaultTxt1 =
            "Click a feature on the map to view info or edit."
        defaultTxt2 =
            "Or use Search tool below."
        defaultTxt3 =
            "Fire ID should be a combination of a year, fire code, and a 6-digit number (e.g., 2019-UTSCS-190031). "           
        defaultTxt4 =
            "IRWIN ID should be a string of alphanumeric characters (e.g., 4d36a069-89b1-4ecb-8a5e-37a0452be635) as obtained from the IRWIN system."           
        defaultTxt5 =
            "Fire Name should be a text value (e.g., Crossing Fire)."           
        testTxt = [defaultTxt3, defaultTxt4, defaultTxt5]
    
    in        
        textColumn [width <| px 350
                , spacing 10, padding 0
                , centerY
                , centerX ]
            [ paragraph
                [ padding 0
                , Font.color <| UI.greenSmoke
                , centerY
                , centerX
                ]
                [text defaultTxt1]            
            , paragraph
                [ padding 0
                , Font.color <| UI.greenSmoke
                , centerY
                , centerX
                ]
                [text defaultTxt2]            
            , el 
                [ width <| px 350
                , height fill
                , centerY
                , centerX
                ]
                <| row 
                    [ width <| px 350
                    , height <| px 30
                    , Bg.color <| UI.white
                    , Border.rounded 5
                    , Border.width 0
                    , centerY
                    , padding 0
                    ]
                    [ Input.text
                        [ El.width <| px 300 
                        , height <| px 7 
                        , Bg.color <| UI.transparent0
                        , Border.width 0
                        , Border.roundEach {topLeft = 5, topRight = 0, bottomLeft = 5, bottomRight = 0}
                        , Font.size <| UI.scaleFont env.windowSize -1
                        -- , Font.color <| UI.white
                        , onEnter EnterWasPressed 
                        ]
                        { onChange = LeftSidebarSearchChanged >> Internal
                        -- , text = ""
                        , text = 
                        case model.leftSearchInput of
                            Just leftSearchInput -> 
                                leftSearchInput
                            _ ->
                                ""
                        -- , placeholder = Nothing
                        , placeholder = Just 
                            <| Input.placeholder 
                                [ height <| px 14                                 
                                , Border.width 0
                                , paddingEach { edges | top = 6, left = 15 }
                                ] 
                            <| text "Enter Fire ID, IRWIN ID, or Fire Name"
                        , label = Input.labelHidden "Dont look at this"
                        }
                    , Input.button 
                        [ Bg.color UI.orangeRoughy
                        , height <| px 30 
                        , paddingXY 15 0
                        , Border.rounded 5
                        ]
                        { 
                        onPress = Just ClickGoButton
                        , label = 
                            el
                                [ Font.color UI.white
                                , UI.montserrat
                                , centerY
                                ]
                            <| text "GO"
                        }
                    ]
                , paragraph
                [ padding 0
                , Font.color <| UI.orangeRoughy
                , Font.size <| UI.scaleFont env.windowSize 0
                , Font.bold
                , centerY
                , centerX
                ]
                [text model.errorMessage]
                , paragraph 
                    [ padding 0
                    , Font.color <| UI.greenSmoke
                    , Font.size <| UI.scaleFont env.windowSize -1
                    , centerY
                    , centerX
                    ]
                    [ textColumn [ spacing 0, padding 0, width <| px 350 ]
                        [ el
                            [ alignLeft
                            , height fill
                            ]
                            (text "-")
                        , paragraph
                            [ paddingEach { edges | top = 0, left = 7 }
                            ]
                            [ text defaultTxt3
                            ]    
                        ]
                    ]       
                , paragraph 
                    [ padding 0
                    , Font.color <| UI.greenSmoke
                    , Font.size <| UI.scaleFont env.windowSize -1
                    , centerY
                    , centerX
                    ]
                    [ textColumn [ spacing 0, padding 0, width <| px 350 ]
                        [ el
                            [ alignLeft
                            , height fill
                            ]
                            (text "-")
                        , paragraph
                            [ paddingEach { edges | top = 0, left = 7 }
                            ]
                            [ text defaultTxt4
                            ]    
                        ]
                    ]       
               , paragraph 
                    [ padding 0
                    , Font.color <| UI.greenSmoke
                    , Font.size <| UI.scaleFont env.windowSize -1
                    , centerY
                    , centerX
                    ]
                    [ textColumn [ spacing 0, padding 0, width <| px 350 ]
                        [ el
                            [ alignLeft
                            , height fill
                            ]
                            (text "-")
                        , paragraph
                            [ paddingEach { edges | top = 0, left = 7 }
                            ]
                            [ text defaultTxt5
                            ]    
                        ]
                    ]       
                  
                
                            
            ]
        

