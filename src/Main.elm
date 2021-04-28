port module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, canvas, video)
import Html.Attributes exposing (src, id, style)

import Process
import Task

import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline exposing (required)
import Html exposing (h3)
import Html exposing (button)
import Html.Events exposing (onClick)

import Element as El exposing (width, height, fill, column, row, Element, padding, spacing, alignTop, rgb, px, text, image, clip, clipX )
import Element.Input as Input
import Element.Border as Border
import Element.Font as Font
import Element.Background as Bg
import Json.Decode.Pipeline exposing (optional)
import Html exposing (Attribute)
import Element exposing (centerX)
import Element exposing (spaceEvenly)
import Element exposing (shrink)
import Html exposing (col)
import Element exposing (centerY)
import Element exposing (wrappedRow)
import Element exposing (maximum)

import UI.Basic as UI
import Url
import Browser.Navigation as Nav
import UI.Basic exposing (windowSizeDecoder)
import Element exposing (minimum)
import Material.List exposing (group)

import Widget
import Widget.Material as Material
import String


import Svg as SVG
import Svg.Attributes as SVG_AT
import Dict exposing (Dict)

---- MODEL ----

port pauseCamera : D.Value -> Cmd msg


port windowResized : (D.Value -> msg) -> Sub msg


type alias Model 
    =
    { state : ModelState
    , key : Nav.Key
    , size : UI.WindowSize
    , usersViewState : UserViewState
    , groupsViewState : GroupsViewState
    , organizationsViewState : OrganizationsViewState
    , permissionsViewState : PermissionsViewState
    , attributesViewState : AttributesViewState
    , partModel : PartModel
    }

type ModelState  
    = Menu
    | Upload UploadState
    | Galary GalaryState
    | UsersView 
    | GroupsView 
    | OrganizationsView 
    | PermissionsView
    | AttributesView
    

type alias User =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    }

type alias Group =
    { id : String
    , name : String
    }

type alias Organization =
    { id : String
    , name : String
    }

type alias Permission =
    { id : String 
    , name : String
    }

type alias UserAttribute =
    { key : String
    , name : String
    }

fakeUsers : List User
fakeUsers =
    [ { id = "abc-def-gh", firstName = "Antonio", lastName = "Montaro", email = "antonia@montaro.com" }
    , { id = "cda-def-gh", firstName = "Ana", lastName = "Bobberson", email = "bobmaster774@gmail.com" }
    , { id = "17z-def-gh", firstName = "Mr", lastName = "Bill", email = "TheRealAlf@yahoo.com" }
    , { id = "9s8-def-gh", firstName = "Thomas", lastName = "Orag", email = "Whereismycar@askjeeves.com" }
    ]

fakeGroups : List Group
fakeGroups =
    [ { id = "asd", name = "Admin" } 
    , { id = "asd", name = "Users" } 
    , { id = "asd", name = "Test" } 
    ]

fakeOrganizations : List Organization
fakeOrganizations =
    [ { id = "asd", name = "The Fun Org" } 
    , { id = "asd", name = "Collective Users United" } 
    , { id = "asd", name = "The Government" } 
    ]

fakePermissions : List Permission
fakePermissions =
    [ { id = "asd", name = "Map:View" }
    , { id = "def", name = "Item:Delete" }
    ]

fakeAttributes : List UserAttribute
fakeAttributes =
    [ { key = "custom:firstName", name = "First Name" }
    , { key = "custom:lastName", name = "Last Name" }
    ]

type alias UserViewState =
    { users : List User
    , sortBy : String 
    , sortDir : Bool  
    , search : Maybe String
    }

type alias GroupsViewState =
    { groups : List Group
    , sortBy : String 
    , sortDir : Bool     
    , search : Maybe String    
    }

type alias OrganizationsViewState =
    { organizations : List Organization
    , sortBy : String 
    , sortDir : Bool     
    , search : Maybe String  
    }

type alias PermissionsViewState =
    { permissions : List Permission
    , sortBy : String
    , sortDir : Bool
    , search : Maybe String
    }

type alias AttributesViewState =
    { attributes : List UserAttribute
    , sortBy : String
    , sortDir : Bool
    , search : Maybe String
    }

initialUsersViewState : UserViewState
initialUsersViewState =
    { users = fakeUsers
    , sortBy = "Last Name"
    , sortDir = True
    , search = Nothing
    }

initialGroupsViewState : GroupsViewState
initialGroupsViewState =
    { groups = fakeGroups
    , sortBy = "Name"
    , sortDir = True
    , search = Nothing
    }

initialOrganizationsViewState : OrganizationsViewState
initialOrganizationsViewState =
    { organizations = fakeOrganizations
    , sortBy = "Name"
    , sortDir = True
    , search = Nothing
    }

initialPermissionsViewState : PermissionsViewState
initialPermissionsViewState =
    { permissions = fakePermissions
    , sortBy = "ID"
    , sortDir = True
    , search = Nothing
    }

initialAttributesViewState : AttributesViewState
initialAttributesViewState =
    { attributes = fakeAttributes
    , sortBy = "ID"
    , sortDir = True
    , search = Nothing
    }

type UploadState 
    = Choose
    | Uploading
    | UploadProcessing
    | UploadSaved

type GalaryState
    = Loading
    | Overview GalaryItems
    | View GalaryItem GalaryItems

type alias GalaryItem =
    { id : String 
    , name : Maybe String
    , iconUrl : String
    --, images : List GalaryItemImage
    , stages : List Stage
    }

type Stage
    = Serial String StageData
    | Parallel String (List StageData)

type alias StageData = 
    { state : StageState
    , config : StageConfig
    }

type StageState 
    = Pending
    | Processing
    | Done Output


type alias StageConfig
    = {}

type Output
    = TextOutput String 
    | ImageOutput GalaryItemImage


type alias GalaryItemImage =
    { url : String
    , urlType : ImageUrlType
    , stepName : String
    }

type ImageUrlType 
    = Static
    | Map

type alias GalaryItems = List GalaryItem

type alias Palette =
    { primary : El.Color
    , secondary : El.Color
    , third : El.Color
    , fourth : El.Color
    , fif : El.Color
    , textPrimary : El.Color
    , textSecondary : El.Color
    , textThird : El.Color
    }


palette : Palette
palette = 
    { primary = El.rgb255 31 27 107
    , secondary = El.rgb255 77 74 136
    , third = El.rgb255 139 134 171
    , fourth = El.rgb255 223 174 155
    , fif = El.rgb255 213 190 107
    , textPrimary = El.rgb255 242 104 148
    , textSecondary = El.rgb255 238 163 187
    , textThird = El.rgb255 241 202 215
    }



init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case D.decodeValue paramDecoder flags of
        Ok val ->
            (   { state = Menu
                , key = key 
                , size = val.size
                , usersViewState = initialUsersViewState
                , groupsViewState = initialGroupsViewState
                , organizationsViewState = initialOrganizationsViewState
                , permissionsViewState = initialPermissionsViewState
                , attributesViewState = initialAttributesViewState
                , partModel = initPartModel
                }
            , Cmd.batch
                [
                ]
            )
        Err e ->
            (   { state = Menu
                , key = key 
                , size = UI.WindowSize 0 0
                , usersViewState = initialUsersViewState
                , groupsViewState = initialGroupsViewState
                , organizationsViewState = initialOrganizationsViewState
                , permissionsViewState = initialPermissionsViewState
                , attributesViewState = initialAttributesViewState
                , partModel = initPartModel
                }
            , Cmd.batch
                [
                ]
            )

type alias Params =
    { size : UI.WindowSize
    }

paramDecoder : D.Decoder Params
paramDecoder =
    D.succeed Params
    |> required "size" windowSizeDecoder

backgroundPalette =
    { color = palette.primary --El.rgb255 124 106 224
    , text = El.rgb 1 1 1
    }

---- UPDATE ----


type Destination
    = Dest_Users
    | Dest_Groups
    | Dest_Orgs
    | Dest_Perms
    | Dest_Attrs

type UsersViewSubMsg
    = UV_NoOp
    | UV_ChangeSort String
    | UV_SearchChange String

type GroupsViewSubMsg
    = GV_NoOp
    | GV_ChangeSort String
    | GV_SearchChange String

type OrganizationsViewSubMsg
    = OV_NoOp
    | OV_ChangeSort String
    | OV_SearchChange String

type PermissionsViewSubMsg
    = PV_NoOp
    | PV_ChangeSort String
    | PV_SearchChange String

type AttributesViewSubMsg
    = AV_NoOp
    | AV_ChangeSort String
    | AV_SearchChange String

type Msg
    = NoOp
    | NavTo Destination
    
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | WindowResized UI.WindowSize

    | Simulate
    | SingleTick
    | AddVel
    

type alias GalaryLoadData =
    { pos : Int
    , items : List GalaryItem
    , more : Bool
    }





-------------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        NavTo dest ->
            case dest of
                Dest_Users ->
                    ( { model | state = UsersView }
                    , Cmd.none
                    )
                Dest_Groups ->
                    ( { model | state = GroupsView }
                    , Cmd.none
                    )
                Dest_Orgs ->
                    ( { model | state = OrganizationsView }
                    , Cmd.none
                    )
                Dest_Perms -> 
                    ( { model | state = PermissionsView }
                    , Cmd.none
                    )
                Dest_Attrs -> 
                    ( { model | state = AttributesView }
                    , Cmd.none
                    )

        


        Simulate ->
            let
                newModel = partTick model.partModel
            in
            ( { model 
                | partModel = newModel
                }
            , Process.sleep 1 |> Task.perform (always Simulate)
            )

        SingleTick ->
            let
                newModel = partTick model.partModel
            in
            ( { model 
                | partModel = newModel
                }
            , Cmd.none
            )

        AddVel ->
            let
                newModel = bounce model.partModel
            in
            ( { model 
                | partModel = newModel
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
    ) 
    |> (\(nm,nc) ->
        case msg of
            WindowResized newSize ->
                ( { nm | size = newSize }, nc )
            _ ->
                (nm, nc)
    )


maxX = 100
maxY = 100

maxRot = 360


updatePosByVel : Int -> Maybe PositionComponent -> Maybe VelocityComponent -> Maybe DragComponent -> (Int, Maybe PositionComponent, Maybe VelocityComponent)
updatePosByVel id mp mv dv =
    let
        dragMul =
            case dv of
                Just drag ->
                    1 - drag.drag
                _ ->
                    1
    in
    case (mp,mv) of
        (Just pos, Just vel) ->
            let
                dx = pos.x + vel.x
                newX = 
                    if dx > maxX then
                        0
                    else if dx < 0 then
                        maxX
                    else 
                        dx

                dy = pos.y + vel.y
                newY = 
                    if dy > maxY then
                        0
                    else if dy < 0 then
                        maxY
                    else 
                        dy


            in
            ( id
            , Just 
                { pos
                | x = newX
                , y = newY
                }
            , Just
                { vel
                | x = vel.x * dragMul
                , y = vel.y * dragMul
                }
            )
        (_,_) -> 
            ( id
            , Nothing
            , mv
            )


updateRotByVel : Int -> Maybe RotationComponent -> Maybe RotationalVelocityComponent -> Maybe DragComponent -> (Int, Maybe RotationComponent, Maybe RotationalVelocityComponent)
updateRotByVel id mp mv dv =
    let
        dragMul =
            case dv of
                Just drag ->
                    1 - drag.drag
                Nothing ->
                    1
    in
    case (mp,mv) of
        (Just pos, Just vel) ->
            let
                drot = pos.rot + vel.rot
                nr = 
                    if drot > maxRot then
                        0
                    else if drot < 0 then
                        maxRot
                    else 
                        drot


            in
            ( id
            , Just 
                { pos
                | rot = nr
                }
            , Just
                { vel
                | rot = vel.rot * dragMul
                }
            )
        (_,_) -> 
            ( id
            , Nothing
            , Nothing
            )


updateEntComponent : Int -> Component -> PartModel -> PartModel
updateEntComponent id component model =
    { model 
    | allComponents =
        getComponents 
            id 
            model
        |> List.map
            (\comp ->
                case (comp, component) of
                    ( Position _, Position np ) ->
                        component
                    ( Velocity _, Velocity nv ) ->
                        component
                    ( Rotation _, Rotation nr ) ->
                        component
                    ( RotationalVelocity _, RotationalVelocity nr ) ->
                        component
                    ( _, _ ) ->
                        comp
                        
            )
        |>  ( \comps ->
                Dict.insert 
                    id 
                    comps 
                    model.allComponents
            )
    }

partTick : PartModel -> PartModel
partTick model =
    let
        particles =
            List.map
                (\id ->
                    getComponents id model
                    |>  ( \entComps -> 
                            { id = id
                            , pos = getPositionComponent entComps
                            , vel = getVelocityComponent entComps
                            , rot = getRotationComponent entComps
                            , rotVel = getRotationalVelocityComponent entComps
                            , drag = getDragComponent entComps
                            }
                        )
                )
                model.ents
            |> List.map 
                ( \nv ->
                    let
                        (_,np,nve) = updatePosByVel nv.id nv.pos nv.vel nv.drag
                        (_,nr,nrve) = updateRotByVel nv.id nv.rot nv.rotVel nv.drag
                    in
                    { id = nv.id
                    , np = np
                    , nr = nr
                    , nv = nve
                    , nrve = nrve
                    }
                )
            |> List.filterMap
                ( \nv ->
                    case (nv.np,nv.nv) of
                        (Just np,Just nve) ->
                            case (nv.nr,nv.nrve) of
                                (Just nr, Just nrve) ->
                                    Just { id = nv.id, pos = Position np, rot = Rotation nr, vel = Velocity nve, rotVel = RotationalVelocity nrve }
                                (_,_) ->
                                    Just { id = nv.id,pos = Position np, rot = Rotation { rot = 0 }, vel = Velocity nve, rotVel = RotationalVelocity { rot = 0 } }
                        _ ->
                            Nothing
                )
            |> List.foldl
                (   \nv -> \mods ->
                        updateEntComponent nv.id nv.pos mods
                        |> updateEntComponent nv.id nv.rot
                        |> updateEntComponent nv.id nv.vel
                        |> updateEntComponent nv.id nv.rotVel
                )
                model
    in
    particles
bounce : PartModel -> PartModel
bounce model =
    let
        particles =
            List.map
                (\id ->
                    getComponents id model
                    |>  ( \entComps -> 
                            { id = id
                            }
                        )
                )
                model.ents
            |> List.foldl
                (   \nv -> \mods ->
                        updateEntComponent nv.id (Velocity { x = 1, y = 1} ) mods
                        |> updateEntComponent nv.id (RotationalVelocity { rot = -21.25 } )
                )
                model
    in
    particles


---- VIEW ----




view : Model -> Browser.Document Msg
view model =
    { title = "Function Dream Image Processing"
    , body =
        [ body model ]
    }

body : Model -> Html Msg
body model =
    El.layout
        [ Bg.color <| backgroundPalette.color
        , Font.color <| backgroundPalette.text
        , Font.size <| UI.scaleWidth model.size 12 50 --UI.scaleFont model.size 1
        ]
    <| El.column
        [ width <| El.maximum 1200 fill
        , El.centerX
        , height fill
        , El.paddingXY 10 20 
        , spacing 25
        ]
        [ El.image 
            [ width <| fill 
            , El.centerX
            ] 
            { src = "./logo.svg", description = "Logo" }
        , El.column
            [ centerX 
            , spacing 3
            ]
            [ El.el 
                [ width <| fill 
                , El.centerX
                , Font.color <| palette.textPrimary
                ] 
                <| El.text "ECS Test"
            ]
        , mainView model
        ]

mainView : Model -> Element Msg
mainView model =
    let
        size = UI.scaleWidth model.size 400 (3850-100)
    in
    El.el 
        [ El.centerX
        , width <| px <| size
        ]
    <| column
        [ width fill
        , height fill
        , spacing 10
        , padding 20
        , Border.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
        ]
        [ svgView model.partModel
        ]


type alias PositionComponent =
    { x : Float
    , y : Float
    }

type alias VelocityComponent =
    { x : Float
    , y : Float
    }

type alias RotationComponent =
    { rot : Float
    }

type alias RotationalVelocityComponent =
    { rot : Float
    }

type alias DragComponent =
    { drag : Float
    }

type Component
    = Position PositionComponent
    | Velocity VelocityComponent
    | Rotation RotationComponent
    | RotationalVelocity RotationalVelocityComponent
    | Drag DragComponent

type alias PartModel =
    { ents : List Int
    , lastId : Int
    , positionComponents : Dict Int PositionComponent
    , velocityComponents : Dict Int VelocityComponent
    , allComponents : Dict Int (List Component)
    }



emptyPartModel : PartModel
emptyPartModel =
    { ents = []
    , lastId = 0
    , positionComponents = Dict.empty
    , velocityComponents = Dict.empty
    , allComponents = Dict.empty
    }



addEntity : PartModel -> ( Int, PartModel )
addEntity model =
    ( model.lastId + 1
    , { model 
        | ents = model.lastId + 1 :: model.ents
        , lastId = model.lastId + 1
        }
    )

addComponent : Int -> Component -> PartModel -> PartModel
addComponent id component model =
    { model
    | allComponents =
        Dict.get id model.allComponents
        |> Maybe.withDefault []
        |> (::) component
        |> ( \itm -> Dict.insert id itm model.allComponents)
    }


onlyPartModel : ( a, PartModel ) -> PartModel
onlyPartModel (_, model ) = model

newPos : Float -> Float -> Component
newPos x y =
    Position { x = x, y = y }

newVel : Float -> Float -> Component
newVel x y =
    Velocity { x = x, y = y }

newRot : Float -> Component
newRot rot =
    Rotation { rot = rot }

newRotVel : Float -> Component
newRotVel rot =
    RotationalVelocity { rot = rot }

newDrag : Float -> Component
newDrag rot =
    Drag { drag = rot }

initPartModel : PartModel
initPartModel =
    emptyPartModel
    |> addEntity
    |>  (\(id, model) ->
            addComponent
                id
                (newPos 20 20)
                model
            |> addComponent
                id
                (newVel 0.25 0.1)
            |> addComponent
                id
                (newRot 0)
            |> addComponent
                id
                (newRotVel -1.25)
            |> addComponent
                id
                (newDrag 0.002)
        )
    |> addEntity
    |>  (\(id, model) ->
            addComponent
                id
                (newPos 10 50)
                model
            |> addComponent
                id
                (newVel -1.005 1.0075)
            |> addComponent
                id
                (newDrag 0.002)
        )
    |> addEntity
    |>  (\(id, model) ->
            addComponent
                id
                (newPos 12 50)
                model
            |> addComponent
                id
                (newVel -0.015 0.0175)
            |> addComponent
                id
                (newRot 45)
            |> addComponent
                id
                (newRotVel 0.25)
            |> addComponent
                id
                (newDrag 0.002)
        )
    |> addEntity
    |>  (\(id, model) ->
            addComponent
                id
                (newPos 14 50)
                model
            |> addComponent
                id
                (newVel -0.05 0.0075)
            |> addComponent
                id
                (newDrag 0.002)
        )

addRandomEnt : PartModel -> PartModel
addRandomEnt mdl =
    mdl 
    |> addEntity
    |>  (\(id, model) ->
            addComponent
                id
                (newPos 14 50)
                model
            |> addComponent
                id
                (newVel -0.05 0.0075)
        )

getComponents : Int -> PartModel -> List Component
getComponents id model =
    Dict.get
        id
        model.allComponents
    |> Maybe.withDefault []

getPositionComponent : List Component -> Maybe PositionComponent
getPositionComponent components =
    List.filterMap
        (\comp ->
            case comp of
                Position pos ->
                    Just pos
                _ ->
                    Nothing
        )
        components
    |> List.head

getRotationComponent : List Component -> Maybe RotationComponent
getRotationComponent components =
    List.filterMap
        (\comp ->
            case comp of
                Rotation pos ->
                    Just pos
                _ ->
                    Nothing
        )
        components
    |> List.head

getVelocityComponent : List Component -> Maybe VelocityComponent
getVelocityComponent components =
    List.filterMap
        (\comp ->
            case comp of
                Velocity pos ->
                    Just pos
                _ ->
                    Nothing
        )
        components
    |> List.head

getRotationalVelocityComponent : List Component -> Maybe RotationalVelocityComponent
getRotationalVelocityComponent components =
    List.filterMap
        (\comp ->
            case comp of
                RotationalVelocity pos ->
                    Just pos
                _ ->
                    Nothing
        )
        components
    |> List.head

getDragComponent : List Component -> Maybe DragComponent
getDragComponent components =
    List.filterMap
        (\comp ->
            case comp of
                Drag pos ->
                    Just pos
                _ ->
                    Nothing
        )
        components
    |> List.head

svgView : PartModel -> Element Msg
svgView model =

    let
        particles =
            List.filterMap
                (\id ->
                    let
                        posComp = 
                            getComponents id model
                            |> getPositionComponent
                        rotComp =
                            getComponents id model
                            |> getRotationComponent
                    in
                        case (posComp,rotComp) of
                            (Just pos, Just rot) ->
                                Just
                                    { x = pos.x
                                    , y = pos.y
                                    , rot = rot.rot
                                    }
                            ( Just pos, _) ->
                                Just
                                    { x = pos.x
                                    , y = pos.y
                                    , rot = 0
                                    }
                            (_,_) ->
                                Nothing
                            


                )
                model.ents

        particleNodes =
            List.map particle particles

        nodes =
            particleNodes ++ []
    in

    column
        [ width fill
        , Border.width 1
        ]
        [ El.html
            <| SVG.svg
                [ SVG_AT.width "520", SVG_AT.height "520", SVG_AT.viewBox "0 0 120 120" ]
                nodes 
        , row
            [ spacing 10
            , padding 5
            ]
            [ buttonBase Simulate (text "Simulate")
            , buttonBase SingleTick (text "Single Tick")
            , buttonBase AddVel (text "Add Velocity")
            ]
        ]



particle : {rec |  x : Float, y : Float, rot : Float } -> SVG.Svg Msg
particle {x,y, rot} =
    let
        transformations =
            [ "translate(" ++ (String.fromFloat x) ++ " " ++ (String.fromFloat y) ++ ")"
            , "rotate(" ++ (String.fromFloat rot) ++ ", 1 0.5)"
            ]
    in
     SVG.polyline 
        [ SVG_AT.x <| String.fromFloat x
        , SVG_AT.y <| String.fromFloat y
        , SVG_AT.transform
            (String.join " " transformations)
        , SVG_AT.width "1"
        , SVG_AT.height "1"
        , SVG_AT.rx "15"
        , SVG_AT.ry "15" 
        , SVG_AT.points "0,1 1,0, 2,1"
        ] 
        []



iconBase : String -> String -> UI.WindowSize -> Element Msg
iconBase  name iconPath size =
    column
        [ width <| px <| UI.scaleWidth size 100 300
        , height <| px <| UI.scaleWidth size 100 300
        , Border.width 1
        , Border.rounded 6
        , padding <| UI.scaleWidth size 10 30
        , Font.center
        ]
        [ El.image 
            [ width <| px <| UI.scaleWidth size 90 140
            , El.centerX
            , Font.color <| rgb 0.5 0.5 0
            ] 
            { src = iconPath, description = name }
        , El.el [ centerX, El.alignBottom ] <| text name
        ]
    

buttonBase : Msg -> Element Msg -> Element Msg
buttonBase action content =
    Input.button
        [ Border.width 1
        , padding 2
        ]
        { onPress = Just <| action
        , label = content
        }


---- PROGRAM ----


main : Program D.Value Model Msg
main =
        Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ windowResized handleResize
        ]

handleResize : D.Value -> Msg
handleResize val =
    case D.decodeValue windowSizeDecoder val of
        Ok newSize ->
            WindowResized newSize
        _ ->
            NoOp