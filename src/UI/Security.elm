module PAM.UI.Security exposing (..)




{- UI -}
import Element as El exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (..)
import Element.Region as Region

{- UIX -}
import PAM.UI.Basic as UI exposing (borders, corners, edges)

{- User -}
import PAM2.User as User

view : User.LoginState -> List User.ACL -> Element msg -> Bool -> Element msg
view user anyPermission viewEl showError =
    if User.hasAnyACL anyPermission user then
        viewEl
    else if showError then
        permissionError anyPermission
    else
        none

permissionError : List String -> Element msg
permissionError required =
    el
        [ Font.color <| rgb 255 0 0 
        , width fill
        , height fill
        , Bg.color <| UI.fadeColor 0.25 <| rgb 255 0 0 
        , Border.width 1
        , padding 10
        , Border.color <| rgb 255 0 0 
        , Border.rounded 6
        ]
    <| textColumn
        [ centerX 
        , width shrink
        , Border.width 1
        , padding 10
        , Border.color <| rgb 255 0 0 
        , spacing 10
        , Border.rounded 6
        ]
    [ paragraph
        [ 
        ]
        [ text "You do not have permission to use this feature."
        ]
    , paragraph
        [ ]
        [ text "Required Permissions:"]
    , textColumn
        [ paddingEach { edges | left = 20 }
        , width shrink
        ]
        <| List.map
            (\itm -> paragraph [ ] [ text itm ])
            required
    ]
