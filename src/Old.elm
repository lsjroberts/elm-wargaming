module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type Msg
    = AddLocation Pointer.Event


type alias Model =
    { locations : List Location
    , teams : List Team
    }


type alias Location =
    { name : String
    , point : ( Int, Int )
    }


type alias Team =
    { name : String }


init : Model
init =
    { locations = []
    , teams = []
    }



-- UPDATE


update msg model =
    case Debug.log "msg" msg of
        AddLocation event ->
            let
                ( x, y ) =
                    event.pointer.offsetPos

                ( intX, intY ) =
                    ( floor x, floor y )
            in
            { model
                | locations =
                    model.locations ++ [ Location "foo" ( intX, intY ) ]
            }



-- VIEW


view model =
    div
        [ style "position" "relative"
        , style "width" "800px"
        , style "height" "600px"
        , style "background-color" "#333333"
        , Pointer.onDown AddLocation
        ]
    <|
        List.map viewLocation model.locations


viewLocation location =
    let
        ( x, y ) =
            location.point
    in
    div
        [ style "position" "absolute"
        , style "left" (String.fromInt (x - 10) ++ "px")
        , style "top" (String.fromInt (y - 10) ++ "px")
        , style "width" "20px"
        , style "height" "20px"
        , style "background-color" "#dddddd"
        ]
        []
