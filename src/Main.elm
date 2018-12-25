module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }



-- MODEL


type Model
    = MainMenu
    | AddPlayers Players
    | ArmyList Players PlayerId



-- | Deploy Deployment Deployment


type alias Players =
    Dict PlayerId Player


type alias PlayerId =
    String


type alias Player =
    { name : String
    , army : Army
    }


type alias Army =
    Dict BattalionId Battalion


type alias BattalionId =
    Int


type alias Battalion =
    { type_ : BaseType
    , quality : Quality
    , bases : List Base
    }


type alias Base =
    { type_ : BaseType
    , quality : Quality
    , elite : Bool
    }


type BaseType
    = Unknown
    | Shot
    | ShotHeavy
    | Mixed
    | PikeHeavy
    | Pike
    | DismountedDragoons
    | MountedDragoons
    | HorseSwedish
    | HorseDutch
    | Artillery


type Quality
    = Raw
    | Trained
    | Veteran


type Contacting
    = Static
    | ChargingFlank
    | ChargingFront
    | ContactingFlank
    | ContactingFront
    | ContactingRear


type Officer
    = Good
    | Average
    | Bad


init : () -> ( Model, Cmd Msg )
init _ =
    -- ( MainMenu, Cmd.none )
    ( ArmyList
        (Dict.fromList
            [ ( "one", Player "Dad" Dict.empty )
            , ( "two", Player "Owen" Dict.empty )
            ]
        )
        "one"
    , Cmd.none
    )


armyScore : Army -> Int
armyScore army =
    Dict.toList army |> List.foldr (\( bid, b ) sum -> sum + battalionScore b) 0


battalionScore : Battalion -> Int
battalionScore b =
    let
        qualityScore =
            case b.quality of
                Raw ->
                    1

                Trained ->
                    2

                Veteran ->
                    3
    in
    qualityScore * List.length b.bases



-- UPDATE


type Msg
    = NoOp
    | Reset
    | GoToAddPlayers
    | SetPlayerName Players PlayerId String
    | GoToArmyList Players PlayerId
    | AddBaseToArmyList Players PlayerId BaseType
    | AddBattalion Players PlayerId BaseType
    | AddBaseToBattalion Players PlayerId BattalionId
    | RemoveBaseFromBattalion Players PlayerId BattalionId


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            init ()

        GoToAddPlayers ->
            ( AddPlayers
                (Dict.fromList
                    [ ( "one", Player "" Dict.empty )
                    , ( "two", Player "" Dict.empty )
                    ]
                )
            , Cmd.none
            )

        SetPlayerName players id name ->
            ( AddPlayers
                (Dict.update id
                    (Maybe.map (\p -> { p | name = name }))
                    players
                )
            , Cmd.none
            )

        GoToArmyList players id ->
            ( ArmyList players id, Cmd.none )

        AddBaseToArmyList players id baseType ->
            let
                player =
                    Dict.get id players

                findBattalion : Army -> Maybe Int
                findBattalion army =
                    Dict.toList army
                        |> List.filter (\( bId, battalion ) -> battalion.type_ == baseType)
                        |> List.head
                        |> Maybe.map (\( bId, _ ) -> bId)

                createBattalion =
                    Battalion baseType Trained [ Base baseType Trained False ]

                updateArmy army =
                    case findBattalion army of
                        Nothing ->
                            Dict.insert (Dict.size army) createBattalion army

                        Just bId ->
                            Dict.update bId (Maybe.map (\b -> { b | bases = Base b.type_ b.quality False :: b.bases })) army

                newPlayers =
                    Dict.update id (Maybe.map (\p -> { p | army = updateArmy p.army })) players
            in
            ( ArmyList newPlayers id, Cmd.none )

        AddBattalion players pid baseType ->
            let
                updateArmy army =
                    Dict.insert (Dict.size army) (Battalion baseType Trained [ Base baseType Trained False ]) army

                newPlayers =
                    Dict.update pid (Maybe.map (\p -> { p | army = updateArmy p.army })) players
            in
            ( ArmyList newPlayers pid, Cmd.none )

        AddBaseToBattalion players pid bid ->
            let
                updateArmy army =
                    Dict.update bid (Maybe.map (\b -> { b | bases = Base b.type_ b.quality False :: b.bases })) army

                newPlayers =
                    Dict.update pid (Maybe.map (\p -> { p | army = updateArmy p.army })) players
            in
            ( ArmyList newPlayers pid, Cmd.none )

        RemoveBaseFromBattalion players pid bid ->
            let
                updateArmy army =
                    Dict.update bid (Maybe.map (\b -> { b | bases = Maybe.withDefault [] (List.tail b.bases) })) army

                newPlayers =
                    Dict.update pid (Maybe.map (\p -> { p | army = updateArmy p.army })) players
            in
            ( ArmyList newPlayers pid, Cmd.none )



-- VIEW


view model =
    layout [ padding 40, height fill ] <|
        case model of
            MainMenu ->
                viewMainMenu

            AddPlayers players ->
                viewAddPlayers players

            ArmyList players id ->
                viewArmyList players id



-- Deployment players deployment ->
--     viewDeployment players deployment


viewMainMenu =
    column [ centerX, centerY, spacing 20 ]
        [ largeButton { onPress = Just GoToAddPlayers, label = text "Create Battle" }
        ]


viewAddPlayers players =
    column [ centerX, centerY, spacing 20 ]
        [ title "Add Players"
        , textInput
            { label = "Player One"
            , onChange = \s -> SetPlayerName players "one" s
            , value =
                Dict.get "one" players
                    |> Maybe.map (\p -> p.name)
                    |> Maybe.withDefault ""
            }
        , textInput
            { label = "Player Two"
            , onChange = \s -> SetPlayerName players "two" s
            , value =
                Dict.get "two" players
                    |> Maybe.map (\p -> p.name)
                    |> Maybe.withDefault ""
            }
        , button
            { onPress = Just (GoToArmyList players "one")
            , label = text "Next →"
            }
        ]


viewArmyList players id =
    let
        maybePlayer =
            Dict.get id players

        squareButton =
            Input.button
                [ Background.color (rgb 0.3 0.3 0.7)
                , Border.rounded 4
                , Font.center
                , Font.color (rgb 1 1 1)
                , Font.size 18
                , padding 10
                , width (px 100)
                , height (px 100)
                ]

        baseButton type_ =
            squareButton
                { onPress = Just (AddBattalion players id type_)
                , label = text (baseToStringShort type_)
                }

        officerButton officer =
            squareButton
                { onPress = Nothing
                , label =
                    column [ centerX, spacing 10, Font.center ]
                        [ el [ width fill ] <| text "Officer"
                        , el [ width fill ] <| text <| officerToString officer
                        ]
                }

        viewBattalion ( bid, battalion ) =
            column [ spacing 16, Background.color (rgb 0.9 0.9 0.9), padding 20 ]
                [ row [ width fill ]
                    [ el [ alignLeft, Font.bold, Font.size 24 ] <| text <| baseToString battalion.type_

                    -- , el [ alignRight, Font.size 24 ] <| text (String.fromInt (bid + 1))
                    ]
                , el [] (text (String.fromInt (battalionScore battalion) ++ " points"))
                , row [ spacing 2 ] <|
                    [ inactiveButton { onPress = Just (RemoveBaseFromBattalion players id bid), label = text " – " }
                    , el
                        [ Background.color (rgb 0.3 0.3 0.3)
                        , Font.center
                        , Font.color (rgb 1 1 1)
                        , paddingXY 20 10
                        ]
                        (text (String.fromInt (List.length battalion.bases)))
                    , button { onPress = Just (AddBaseToBattalion players id bid), label = text " + " }
                    ]
                , row [ spacing 8 ]
                    [ optionButtons
                        [ ( Raw, "Raw" ), ( Trained, "Trained" ), ( Veteran, "Veteran" ) ]
                        Trained
                    ]
                ]
    in
    case maybePlayer of
        Nothing ->
            text "ERROR"

        Just player ->
            column [ width fill, spacing 20 ]
                [ title (player.name ++ " – Choose Your Army")
                , el [ centerX ] <| text ("Score: " ++ String.fromInt (armyScore player.army))
                , row [ width fill, spacing 20 ]
                    [ column
                        [ alignTop
                        , width fill
                        , spacing 20
                        ]
                        [ heading "Add a Battalion"
                        , wrappedRow [ spacing 20 ]
                            [ baseButton Shot
                            , baseButton ShotHeavy
                            , baseButton Mixed
                            , baseButton PikeHeavy
                            , baseButton Pike
                            , baseButton MountedDragoons
                            , baseButton HorseSwedish
                            , baseButton HorseDutch
                            , baseButton Artillery
                            , officerButton Good
                            , officerButton Average
                            , officerButton Bad
                            ]
                        ]
                    , wrappedRow [ alignTop, width (fillPortion 3), spacing 20 ] <|
                        (player.army
                            |> Dict.toList
                            |> List.map viewBattalion
                        )
                    ]
                ]


title t =
    el
        [ Font.bold
        , Font.size 32
        , centerX
        , paddingEach { top = 0, right = 0, bottom = 30, left = 0 }
        ]
        (text t)


heading t =
    el
        [ Font.bold
        , Font.size 24
        , centerX
        , paddingEach { top = 0, right = 0, bottom = 16, left = 0 }
        ]
        (text t)


button =
    Input.button
        [ Background.color (rgb 0.3 0.3 0.7)
        , Border.rounded 4
        , Font.center
        , Font.color (rgb 1 1 1)
        , padding 10
        , width fill
        ]


inactiveButton =
    Input.button
        [ Background.color (rgb 0.7 0.7 0.7)
        , Border.rounded 4
        , Font.center
        , Font.color (rgb 1 1 1)
        , padding 10
        , width fill
        ]


largeButton =
    Input.button
        [ Background.color (rgb 0.3 0.3 0.7)
        , Border.rounded 4
        , Font.center
        , Font.color (rgb 1 1 1)
        , Font.size 48
        , padding 30
        , width fill
        ]


optionButtons options value =
    row [ spacing 2, width fill ] <|
        List.map
            (\( v, t ) ->
                (if value == v then
                    button

                 else
                    inactiveButton
                )
                    { onPress = Nothing --Just (msg v)
                    , label = text t
                    }
            )
            options


modifierButton label isActive =
    row [ spacing 20, width fill ]
        [ el [ alignRight ] <|
            (if isActive then
                button

             else
                inactiveButton
            )
                { onPress = Nothing --Just msg
                , label = text label
                }
        ]


textInput { label, onChange, value } =
    Input.text []
        { label = Input.labelAbove [] (text label)
        , onChange = onChange
        , placeholder = Nothing
        , text = value
        }


baseToString base =
    case base of
        Unknown ->
            "Unknown"

        Shot ->
            "Shot"

        ShotHeavy ->
            "Shot Heavy"

        Mixed ->
            "Mixed"

        PikeHeavy ->
            "Pike Heavy"

        Pike ->
            "Pike"

        DismountedDragoons ->
            "Dismounted Dragoons"

        MountedDragoons ->
            "Mounted Dragoons"

        HorseSwedish ->
            "Horse Swedish"

        HorseDutch ->
            "Horse Dutch"

        Artillery ->
            "Artillery"


baseToStringShort base =
    case base of
        Unknown ->
            "?"

        Shot ->
            "Foot (S)"

        ShotHeavy ->
            "Foot (SH)"

        Mixed ->
            "Foot (M)"

        PikeHeavy ->
            "Foot (PH)"

        Pike ->
            "Foot (P)"

        DismountedDragoons ->
            "Dragoons"

        MountedDragoons ->
            "Dragoons"

        HorseSwedish ->
            "Horse (S)"

        HorseDutch ->
            "Horse (D)"

        Artillery ->
            "Artillery"


officerToString officer =
    case officer of
        Good ->
            "Good"

        Average ->
            "Average"

        Bad ->
            "Bad"
