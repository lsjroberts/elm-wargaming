module Polemos.Combat exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Random


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }



-- MODEL


type Model
    = New
    | Ranged RangedCombat
    | Close CloseCombat



-- | Charging ChargingCombat


type alias RangedCombat =
    { offensive : BaseType
    , defensive : BaseType
    , offensiveModifiers : RangedModifiers
    , defensiveModifiers : RangedModifiers
    , offensiveRoll : Maybe Int
    , defensiveRoll : Maybe Int
    }


type alias CloseCombat =
    { offensive : BaseType
    , defensive : BaseType

    -- , counts : CloseCounts
    , offensiveModifiers : CloseModifiers
    , defensiveModifiers : CloseModifiers
    , offensiveRoll : Maybe Int
    , defensiveRoll : Maybe Int
    }


type alias ChargingCombat =
    { defensive : BaseType
    , offensiveModifiers : ChargingModifiers
    , defensiveModifiers : ChargingModifiers
    , offensiveRoll : Maybe Int
    , defensiveRoll : Maybe Int
    }


type Position
    = Offensive
    | Defensive


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


type alias RangedModifiers =
    { longRange : Bool
    , artilleryDistance : Int
    , shaken : Int
    , quality : Quality
    , elite : Bool
    , officerAttached : Bool

    -- , terrain : Terrain
    , uphill : Bool
    , saved : Bool
    }


type alias CloseModifiers =
    { contacting : Contacting
    , flanksOverlapped : Int
    , shaken : Int
    , recoiled : Bool
    , officerAttached : Maybe Officer
    , quality : Quality
    , elite : Bool
    , firstPhase : Bool
    , terrainDefence : Int
    , saved : Bool
    }


type alias ChargingModifiers =
    { shaken : Int
    , quality : Quality
    , elite : Bool

    -- , terrain : Terrain
    , saved : Bool
    }


type alias CloseCounts =
    { offensive : Int
    , defensive : Int
    , overlap : Int
    , saved : Bool
    }


type Quality
    = Raw
    | Trained
    | Veteran


type Contacting
    = Static
    | ChargingFlankOrRear
    | ChargingFront
    | ContactingFlankOrRear
    | ContactingFront


type Officer
    = Good
    | Average
    | Bad


type Msg
    = Reset
      -- Ranged
    | StartRanged
    | SetRangedOffensive RangedCombat BaseType
    | SetRangedDefensive RangedCombat BaseType
    | SetRangedOffensiveModifiers RangedCombat RangedModifiers
    | SetRangedDefensiveModifiers RangedCombat RangedModifiers
    | SetRangedOffensiveRoll RangedCombat Int
    | SetRangedDefensiveRoll RangedCombat Int
    | RandomRangedOffensiveRoll RangedCombat
    | RandomRangedDefensiveRoll RangedCombat
    | ResetRangedRolls RangedCombat
      -- Close
    | StartClose
      -- | SetCloseCounts CloseCombat CloseCounts
    | SetCloseOffensive CloseCombat BaseType
    | SetCloseDefensive CloseCombat BaseType
    | SetCloseOffensiveModifiers CloseCombat CloseModifiers
    | SetCloseDefensiveModifiers CloseCombat CloseModifiers
    | SetCloseOffensiveRoll CloseCombat Int
    | SetCloseDefensiveRoll CloseCombat Int
    | ResetCloseRolls CloseCombat



-- Charging
-- | StartCharging
-- | SetChargingDefensive ChargingCombat BaseType
-- | SetChargingOffensiveModifiers ChargingCombat ChargingModifiers
-- | SetChargingDefensiveModifiers ChargingCombat ChargingModifiers


init : () -> ( Model, Cmd Msg )
init _ =
    ( New, Cmd.none )


initRangedModifiers : RangedModifiers
initRangedModifiers =
    { longRange = False
    , artilleryDistance = 0
    , shaken = 0
    , quality = Trained
    , elite = False
    , officerAttached = False

    -- , terrain : Terrain
    , uphill = False
    , saved = False
    }


initCloseModifiers : CloseModifiers
initCloseModifiers =
    { contacting = Static
    , flanksOverlapped = 0
    , shaken = 0
    , recoiled = False
    , officerAttached = Nothing
    , quality = Trained
    , elite = False
    , firstPhase = True
    , terrainDefence = 0
    , saved = False
    }


rangedScore ranged position =
    let
        modifiers =
            case position of
                Offensive ->
                    ranged.offensiveModifiers

                Defensive ->
                    ranged.defensiveModifiers

        rollScore =
            case position of
                Offensive ->
                    Maybe.withDefault 0 ranged.offensiveRoll

                Defensive ->
                    Maybe.withDefault 0 ranged.defensiveRoll

        baseScore =
            case position of
                Offensive ->
                    case ranged.offensive of
                        Unknown ->
                            0

                        Shot ->
                            5

                        ShotHeavy ->
                            4

                        Mixed ->
                            3

                        PikeHeavy ->
                            2

                        Pike ->
                            0

                        DismountedDragoons ->
                            2

                        MountedDragoons ->
                            1

                        HorseSwedish ->
                            3

                        HorseDutch ->
                            0

                        Artillery ->
                            4

                Defensive ->
                    case ranged.defensive of
                        Unknown ->
                            0

                        Shot ->
                            5

                        ShotHeavy ->
                            4

                        Mixed ->
                            3

                        PikeHeavy ->
                            2

                        Pike ->
                            2

                        DismountedDragoons ->
                            3

                        MountedDragoons ->
                            3

                        HorseSwedish ->
                            2

                        HorseDutch ->
                            2

                        Artillery ->
                            2
    in
    baseScore
        + rollScore
        |> (+)
            (if modifiers.longRange then
                -4

             else
                0
            )
        |> (+)
            (if modifiers.artilleryDistance > 1 then
                modifiers.artilleryDistance * -1

             else
                0
            )
        |> (+) (modifiers.shaken * -1)
        |> (+)
            (if modifiers.quality == Raw then
                -1

             else
                0
            )
        |> (+)
            (if modifiers.quality == Veteran then
                1

             else
                0
            )
        |> (+)
            (if modifiers.elite then
                1

             else
                0
            )
        |> (+)
            (if modifiers.officerAttached then
                1

             else
                0
            )
        |> (+)
            (if modifiers.uphill then
                1

             else
                0
            )


closeScore : CloseCombat -> Position -> Int
closeScore close position =
    let
        modifiers : CloseModifiers
        modifiers =
            case position of
                Offensive ->
                    close.offensiveModifiers

                Defensive ->
                    close.defensiveModifiers

        rollScore =
            case position of
                Offensive ->
                    Maybe.withDefault 0 close.offensiveRoll

                Defensive ->
                    Maybe.withDefault 0 close.defensiveRoll

        baseScore =
            case position of
                Offensive ->
                    case close.offensive of
                        Unknown ->
                            0

                        Shot ->
                            1

                        ShotHeavy ->
                            2

                        Mixed ->
                            3

                        PikeHeavy ->
                            4

                        Pike ->
                            5

                        DismountedDragoons ->
                            1

                        MountedDragoons ->
                            1

                        HorseSwedish ->
                            3

                        HorseDutch ->
                            3

                        Artillery ->
                            0

                Defensive ->
                    case close.defensive of
                        Unknown ->
                            0

                        Shot ->
                            1

                        ShotHeavy ->
                            2

                        Mixed ->
                            3

                        PikeHeavy ->
                            4

                        Pike ->
                            5

                        DismountedDragoons ->
                            1

                        MountedDragoons ->
                            1

                        HorseSwedish ->
                            3

                        HorseDutch ->
                            3

                        Artillery ->
                            1

        contactingScore =
            case modifiers.contacting of
                Static ->
                    0

                ChargingFlankOrRear ->
                    6

                ContactingFlankOrRear ->
                    4

                ChargingFront ->
                    2

                ContactingFront ->
                    1
    in
    baseScore
        + rollScore
        + contactingScore
        - modifiers.flanksOverlapped
        - (modifiers.shaken * 2)
        + modifiers.terrainDefence
        |> (+)
            (if modifiers.recoiled then
                -2

             else
                0
            )
        |> (+)
            (if modifiers.officerAttached == Just Good then
                2

             else if modifiers.officerAttached == Just Average then
                1

             else
                0
            )
        |> (+)
            (if modifiers.firstPhase && (modifiers.quality == Veteran || modifiers.elite) then
                1

             else
                0
            )
        |> (+)
            (if modifiers.quality == Raw then
                -1

             else
                0
            )


rangedScoreEffect ranged score =
    if score <= 1 then
        "No effect"

    else if score <= 3 then
        "Target halts"

    else if ranged.defensiveModifiers.shaken >= 2 then
        "Target routs"

    else if score <= 5 then
        "Target halts, shaken +1"

    else
        "Target recoils, shaken +1"


closeScoreEffect close score =
    if score == 0 then
        "Both recoil"

    else if close.defensiveModifiers.recoiled then
        "Recoil twice → Rout"

    else if score == 1 then
        "Recoil"

    else if score <= 3 then
        "Recoil shaken +1"

    else if close.defensiveModifiers.shaken >= 2 then
        "Shaken over 2 → Rout"

    else if score <= 5 then
        "Recoil shaken +2"

    else
        "Rout"



-- UPDATE


update msg model =
    case Debug.log "" msg of
        Reset ->
            ( New, Cmd.none )

        -- Ranged
        StartRanged ->
            ( Ranged (RangedCombat Unknown Unknown initRangedModifiers initRangedModifiers Nothing Nothing), Cmd.none )

        SetRangedOffensive ranged base ->
            ( Ranged { ranged | offensive = base }, Cmd.none )

        SetRangedDefensive ranged base ->
            ( Ranged { ranged | defensive = base }, Cmd.none )

        SetRangedOffensiveModifiers ranged modifiers ->
            ( Ranged { ranged | offensiveModifiers = modifiers }, Cmd.none )

        SetRangedDefensiveModifiers ranged modifiers ->
            ( Ranged { ranged | defensiveModifiers = modifiers }, Cmd.none )

        SetRangedOffensiveRoll ranged roll ->
            ( Ranged { ranged | offensiveRoll = Just roll }, Cmd.none )

        SetRangedDefensiveRoll ranged roll ->
            ( Ranged { ranged | defensiveRoll = Just roll }, Cmd.none )

        RandomRangedOffensiveRoll ranged ->
            ( model, Random.generate (SetRangedOffensiveRoll ranged) rollD6 )

        RandomRangedDefensiveRoll ranged ->
            ( model, Random.generate (SetRangedDefensiveRoll ranged) rollD6 )

        ResetRangedRolls ranged ->
            ( Ranged { ranged | offensiveRoll = Nothing, defensiveRoll = Nothing }, Cmd.none )

        -- Close
        StartClose ->
            ( Close (CloseCombat Unknown Unknown initCloseModifiers initCloseModifiers Nothing Nothing), Cmd.none )

        -- SetCloseCounts close counts ->
        --     ( Close { close | counts = counts }, Cmd.none )
        SetCloseOffensive close base ->
            ( Close { close | offensive = base }, Cmd.none )

        SetCloseDefensive close base ->
            ( Close { close | defensive = base }, Cmd.none )

        SetCloseOffensiveModifiers close modifiers ->
            ( Close { close | offensiveModifiers = modifiers }, Cmd.none )

        SetCloseDefensiveModifiers close modifiers ->
            ( Close { close | defensiveModifiers = modifiers }, Cmd.none )

        SetCloseOffensiveRoll close roll ->
            ( Close { close | offensiveRoll = Just roll }, Cmd.none )

        SetCloseDefensiveRoll close roll ->
            ( Close { close | defensiveRoll = Just roll }, Cmd.none )

        ResetCloseRolls close ->
            ( Close { close | offensiveRoll = Nothing, defensiveRoll = Nothing }, Cmd.none )



-- Charging
-- StartCharging ->
--     ( Charging (ChargingCombat Unknown (ChargingModifiers 0 Trained False False) (ChargingModifiers 0 Trained False False) Nothing Nothing), Cmd.none )
-- SetChargingDefensive close base ->
--     ( Charging { close | defensive = base }, Cmd.none )
-- SetChargingOffensiveModifiers charging modifiers ->
--     ( Charging { charging | offensiveModifiers = modifiers }, Cmd.none )
-- SetChargingDefensiveModifiers charging modifiers ->
--     ( Charging { charging | defensiveModifiers = modifiers }, Cmd.none )


rollD6 : Random.Generator Int
rollD6 =
    Random.int 1 6



-- VIEW


view model =
    Element.layout [ padding 40, height fill ] <|
        case model of
            New ->
                viewNew

            Ranged ranged ->
                viewRanged ranged

            Close close ->
                viewClose close



-- Charging charging ->
--     viewCharging charging


viewNew =
    column [ centerX, spacing 20 ]
        [ title "Combat"
        , button { onPress = Just StartRanged, label = text "Ranged Combat" }
        , button { onPress = Just StartClose, label = text "Close Combat" }

        -- , button { onPress = Just StartCharging, label = text "Charging" }
        ]


viewRanged ranged =
    let
        calculator =
            if ranged.offensive == Unknown then
                viewBaseButtons ranged SetRangedOffensive "Offensive Base" "Combat = Ranged"

            else if ranged.defensive == Unknown then
                viewBaseButtons ranged SetRangedDefensive "Defensive Base" "Combat = Ranged"

            else if not ranged.offensiveModifiers.saved then
                viewRangedOffensiveModifiers ranged

            else if not ranged.defensiveModifiers.saved then
                viewRangedDefensiveModifiers ranged

            else if ranged.offensiveRoll == Nothing then
                viewRangedOffensiveRoll ranged

            else if ranged.defensiveRoll == Nothing then
                viewRangedDefensiveRoll ranged

            else
                viewRangedScores ranged
    in
    row [ spacing 20, width fill ]
        [ el [ width (fillPortion 1) ] <| el [ centerX ] <| calculator

        -- , el [ width (fillPortion 1), centerY ] <| el [ centerX ] <| viewRangedBattle ranged
        ]



-- viewCharging charging =
--     let
--         calculator =
--             if charging.defensive == Unknown then
--                 viewBaseButtons charging SetChargingDefensive "Defensive Base" "Combat = Charging"
--             else if not charging.offensiveModifiers.saved then
--                 viewChargingOffensiveModifiers charging
--             else if not charging.defensiveModifiers.saved then
--                 viewChargingDefensiveModifiers charging
--             else if charging.offensiveRoll == Nothing then
--                 viewChargingOffensiveRoll charging
--             else if charging.defensiveRoll == Nothing then
--                 viewChargingDefensiveRoll charging
--             else
--                 viewRangedScores charging
--     in
--     row [ spacing 20, width fill, height fill ]
--         [ el [ width (fillPortion 1) ] <| el [ centerX ] <| calculator
--         -- , el [ width (fillPortion 1), centerY ] <| el [ centerX ] <| viewRangedBattle charging
--         ]


viewRangedBattle ranged =
    let
        viewIcon color label =
            el
                [ width (px 28)
                , height (px 28)
                , Background.color color
                , Border.rounded 14
                , Font.color (rgb 1 1 1)
                , Font.center
                , padding 4
                ]
                (text label)

        viewBase base modifiers score color =
            column [ spacing 5 ]
                [ row [ width fill, spaceEvenly ]
                    [ if modifiers.shaken > 0 then
                        row [ spacing 5 ]
                            (List.range 1 modifiers.shaken
                                |> List.map (\_ -> viewIcon (rgb 0.4 0 0) "S")
                            )

                      else
                        none
                    , if modifiers.elite then
                        viewIcon (rgb 0 0 0.4) "E"

                      else
                        none
                    , if modifiers.officerAttached then
                        viewIcon (rgb 0 0 0) "O"

                      else
                        none
                    , viewIcon
                        (case modifiers.quality of
                            Raw ->
                                rgb 0.4 0 0

                            Trained ->
                                rgb 0.4 0.4 0.4

                            Veteran ->
                                rgb 0 0.4 0
                        )
                        (case modifiers.quality of
                            Raw ->
                                "R"

                            Trained ->
                                "T"

                            Veteran ->
                                "V"
                        )
                    ]
                , el [ width (px 240), height (px 120), Background.color color ] <|
                    column [ centerX, centerY, spacing 10, Font.color (rgb 1 1 1) ] <|
                        [ text (baseToString base)
                        , el [ centerX ] <| text (String.fromInt score)
                        ]
                ]
    in
    column
        [ spacing
            (if ranged.offensiveModifiers.longRange then
                240

             else
                120
            )
        ]
        [ viewBase ranged.offensive ranged.offensiveModifiers (rangedScore ranged Offensive) (rgb 0.5 0.8 0.5)
        , viewBase ranged.defensive ranged.defensiveModifiers (rangedScore ranged Defensive) (rgb 0.8 0.5 0.5)
        ]


viewClose close =
    let
        calculator =
            -- if close.counts.saved == False then
            --     viewCloseCounts close
            -- else
            if close.offensive == Unknown then
                viewBaseButtons close SetCloseOffensive "Offensive Base" "Combat = Close"

            else if close.defensive == Unknown then
                viewBaseButtons close SetCloseDefensive "Defensive Base" "Combat = Close"

            else if not close.offensiveModifiers.saved then
                viewCloseOffensiveModifiers close

            else if not close.defensiveModifiers.saved then
                viewCloseDefensiveModifiers close

            else if close.offensiveRoll == Nothing then
                viewCloseOffensiveRoll close

            else if close.defensiveRoll == Nothing then
                viewCloseDefensiveRoll close

            else
                viewCloseScores close

        -- text "no"
    in
    row [ spacing 20, width fill ]
        [ el [ width (fillPortion 1) ] <| el [ centerX ] <| calculator

        -- , el [ width (fillPortion 1), centerY ] <| el [ centerX ] <| viewCloseBattle close
        ]


viewCloseBattle close =
    let
        baseIcon =
            el [ width (px 80), height (px 40), Background.color (rgb 0.5 0.5 0.5) ] <| none
    in
    column [ spacing 40 ]
        [ row [ spacing 2, moveRight (toFloat close.counts.overlap * 41) ] <| List.map (\i -> baseIcon) (List.range 1 close.counts.offensive)
        , row [ spacing 2 ] <| List.map (\i -> baseIcon) (List.range 1 close.counts.defensive)
        ]


viewBaseButtons combat setMsg label subLabel =
    let
        baseButton base =
            button
                { onPress = Just (setMsg combat base)
                , label = text (baseToString base)
                }
    in
    column [ spacing 20 ]
        [ title label

        -- , option subLabel
        -- , if combat.offensive == Unknown then
        --     none
        --   else
        --     option ("Offensive = " ++ baseToString combat.offensive)
        -- , separator
        , row [ spacing 20 ]
            [ column [ spacing 20 ]
                [ baseButton Shot
                , baseButton ShotHeavy
                , baseButton Mixed
                , baseButton PikeHeavy
                , baseButton Pike
                ]
            , column [ spacing 20 ]
                [ baseButton DismountedDragoons
                , baseButton MountedDragoons
                , baseButton HorseSwedish
                , baseButton HorseDutch
                , baseButton Artillery
                ]
            ]
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        ]



-- viewCloseCounts close =
--     let
--         numberedButton_ label numbers get action =
--             numberedButton label numbers (get close) (\i -> SetCloseCounts close (action close.counts i))
--     in
--     column [ spacing 20 ]
--         [ title "Number of Bases"
--         , option "Combat = Close"
--         , separator
--         , numberedButton_ "Offensive" [ 0, 1, 2, 3, 4, 5, 6 ] (.counts >> .offensive) (\c i -> { c | offensive = i })
--         , numberedButton_ "Defensive" [ 0, 1, 2, 3, 4, 5, 6 ] (.counts >> .defensive) (\c i -> { c | defensive = i })
--         , row [ spacing 10, width fill ]
--             [ text "Overlap"
--             , row [ spacing 10, alignRight ]
--                 [ button { onPress = Just (SetCloseCounts close ((\counts -> { counts | overlap = counts.overlap - 1 }) close.counts)), label = text "←" }
--                 , button { onPress = Just (SetCloseCounts close ((\counts -> { counts | overlap = counts.overlap + 1 }) close.counts)), label = text "→" }
--                 ]
--             ]
--         , button { onPress = Just (SetCloseCounts close ((\counts -> { counts | saved = True }) close.counts)), label = text "Next →" }
--         ]


viewRangedOffensiveModifiers ranged =
    let
        modifierButton_ label isActive action =
            modifierButton label (isActive ranged.offensiveModifiers) (SetRangedOffensiveModifiers ranged (action ranged.offensiveModifiers))

        numberedButton_ label numbers get action =
            numberedButton label numbers (get ranged.offensiveModifiers) (\i -> SetRangedOffensiveModifiers ranged (action ranged.offensiveModifiers i))

        optionButtons_ label options get action =
            optionButtons label options (get ranged.offensiveModifiers) (\v -> SetRangedOffensiveModifiers ranged (action ranged.offensiveModifiers v))
    in
    column [ spacing 20 ]
        [ title "Offensive Modifiers"

        -- , option "Combat = Ranged"
        -- , option ("Offensive = " ++ baseToString ranged.offensive)
        -- , option ("Defensive = " ++ baseToString ranged.defensive)
        -- , separator
        , if ranged.offensive == Artillery then
            numberedButton_ "Artillery Distance" [ 0, 1, 2, 3, 4 ] .artilleryDistance (\m i -> { m | artilleryDistance = i })

          else
            modifierButton_ "Long Range" .longRange (\m -> { m | longRange = not m.longRange })
        , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , optionButtons_ "Quality" [ ( Raw, "Raw" ), ( Trained, "Trained" ), ( Veteran, "Veteran" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , modifierButton_ "Officer Attached" .officerAttached (\m -> { m | officerAttached = not m.officerAttached })
        , modifierButton_ "Uphill of target" .uphill (\m -> { m | uphill = not m.uphill })
        , button { onPress = Just (SetRangedOffensiveModifiers ranged (ranged.offensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        ]


viewRangedDefensiveModifiers ranged =
    let
        modifierButton_ label isActive action =
            modifierButton label (isActive ranged.defensiveModifiers) (SetRangedDefensiveModifiers ranged (action ranged.defensiveModifiers))

        numberedButton_ label numbers get action =
            numberedButton label numbers (get ranged.defensiveModifiers) (\i -> SetRangedDefensiveModifiers ranged (action ranged.defensiveModifiers i))

        optionButtons_ label options get action =
            optionButtons label options (get ranged.defensiveModifiers) (\v -> SetRangedDefensiveModifiers ranged (action ranged.defensiveModifiers v))
    in
    column [ spacing 20 ]
        [ title "Defensive Modifiers"
        , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , optionButtons_ "Quality" [ ( Raw, "Raw" ), ( Trained, "Trained" ), ( Veteran, "Veteran" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , button { onPress = Just (SetRangedDefensiveModifiers ranged (ranged.defensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        ]


viewCloseOffensiveModifiers close =
    let
        modifierButton_ label isActive action =
            modifierButton label (isActive close.offensiveModifiers) (SetCloseOffensiveModifiers close (action close.offensiveModifiers))

        numberedButton_ label numbers get action =
            numberedButton label numbers (get close.offensiveModifiers) (\i -> SetCloseOffensiveModifiers close (action close.offensiveModifiers i))

        optionButtons_ label options get action =
            optionButtons label options (get close.offensiveModifiers) (\v -> SetCloseOffensiveModifiers close (action close.offensiveModifiers v))
    in
    column [ centerX, spacing 20 ]
        [ title "Offensive Modifiers"
        , modifierButton_ "First Phase" .firstPhase (\m -> { m | firstPhase = not m.firstPhase })
        , optionButtons_ "Contacting" [ ( ContactingFront, "Front" ), ( ContactingFlankOrRear, "Flank / Rear" ), ( Static, "None" ) ] .contacting (\m v -> { m | contacting = v })
        , optionButtons_ "Charging" [ ( ChargingFront, "Front" ), ( ChargingFlankOrRear, "Flank / Rear" ), ( Static, "None" ) ] .contacting (\m v -> { m | contacting = v })
        , numberedButton_ "Flanks Overlapped" [ 0, 1, 2 ] .flanksOverlapped (\m i -> { m | flanksOverlapped = i })
        , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , modifierButton_ "Recoiled" .recoiled (\m -> { m | recoiled = not m.recoiled })
        , optionButtons_ "Quality" [ ( Raw, "Raw" ), ( Trained, "Trained" ), ( Veteran, "Veteran" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , optionButtons_ "Officer" [ ( Nothing, "None" ), ( Just Good, "Good" ), ( Just Average, "Average" ), ( Just Bad, "Bad" ) ] .officerAttached (\m v -> { m | officerAttached = v })
        , button { onPress = Just (SetCloseOffensiveModifiers close (close.offensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        ]


viewCloseDefensiveModifiers close =
    let
        modifierButton_ label isActive action =
            modifierButton label (isActive close.defensiveModifiers) (SetCloseDefensiveModifiers close (action close.defensiveModifiers))

        numberedButton_ label numbers get action =
            numberedButton label numbers (get close.defensiveModifiers) (\i -> SetCloseDefensiveModifiers close (action close.defensiveModifiers i))

        optionButtons_ label options get action =
            optionButtons label options (get close.defensiveModifiers) (\v -> SetCloseDefensiveModifiers close (action close.defensiveModifiers v))
    in
    column [ centerX, spacing 20 ]
        [ title "Defensive Modifiers"
        , numberedButton_ "Flanks Overlapped" [ 0, 1, 2 ] .flanksOverlapped (\m i -> { m | flanksOverlapped = i })
        , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , modifierButton_ "Recoiled" .recoiled (\m -> { m | recoiled = not m.recoiled })
        , optionButtons_ "Quality" [ ( Raw, "Raw" ), ( Trained, "Trained" ), ( Veteran, "Veteran" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , optionButtons_ "Officer" [ ( Nothing, "None" ), ( Just Good, "Good" ), ( Just Average, "Average" ), ( Just Bad, "Bad" ) ] .officerAttached (\m v -> { m | officerAttached = v })
        , numberedButton_ "Terrain" [ 0, 1, 2 ] .terrainDefence (\m i -> { m | terrainDefence = i })
        , button { onPress = Just (SetCloseDefensiveModifiers close (close.defensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        ]



-- viewChargingOffensiveModifiers charging =
--     let
--         modifierButton_ label isActive action =
--             modifierButton label (isActive charging.offensiveModifiers) (SetChargingDefensiveModifiers charging (action charging.offensiveModifiers))
--         numberedButton_ label numbers get action =
--             numberedButton label numbers (get charging.offensiveModifiers) (\i -> SetChargingDefensiveModifiers charging (action charging.offensiveModifiers i))
--         optionButtons_ label options get action =
--             optionButtons label options (get charging.offensiveModifiers) (\v -> SetChargingDefensiveModifiers charging (action charging.offensiveModifiers v))
--     in
--     column [ spacing 20 ]
--         [ title "Offensive Modifiers"
--         , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
--         , optionButtons_ "Quality" [ ( Raw, "Raw" ), ( Trained, "Trained" ), ( Veteran, "Veteran" ) ] .quality (\m v -> { m | quality = v })
--         , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
--         , button { onPress = Just (SetChargingOffensiveModifiers charging (charging.offensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
--         , separator
--         , inactiveButton { onPress = Just Reset, label = text "New" }
--         ]
-- viewChargingDefensiveModifiers charging =
--     let
--         modifierButton_ label isActive action =
--             modifierButton label (isActive charging.defensiveModifiers) (SetChargingDefensiveModifiers charging (action charging.defensiveModifiers))
--         numberedButton_ label numbers get action =
--             numberedButton label numbers (get charging.defensiveModifiers) (\i -> SetChargingDefensiveModifiers charging (action charging.defensiveModifiers i))
--         optionButtons_ label options get action =
--             optionButtons label options (get charging.defensiveModifiers) (\v -> SetChargingDefensiveModifiers charging (action charging.defensiveModifiers v))
--     in
--     column [ spacing 20 ]
--         [ title "Offensive Modifiers"
--         , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
--         , optionButtons_ "Quality" [ ( Raw, "Raw" ), ( Trained, "Trained" ), ( Veteran, "Veteran" ) ] .quality (\m v -> { m | quality = v })
--         , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
--         , button { onPress = Just (SetChargingDefensiveModifiers charging (charging.defensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
--         , separator
--         , inactiveButton { onPress = Just Reset, label = text "New" }
--         ]


viewRangedOffensiveRoll ranged =
    column [ spacing 20 ] <|
        [ title "Offensive Roll"
        , button
            { onPress = Just (RandomRangedOffensiveRoll ranged)
            , label = text "Roll"
            }
        , separator
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetRangedOffensiveRoll ranged i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 1, 2, 3 ]
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetRangedOffensiveRoll ranged i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 4, 5, 6 ]
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        , separator
        , option "Combat = Ranged"
        , option ("Offensive = " ++ baseToString ranged.offensive)
        , option ("Defensive = " ++ baseToString ranged.defensive)
        , separator
        , option ("Offensive Plain Score = " ++ (rangedScore ranged Offensive |> String.fromInt))
        , option ("Defensive Plain Score = " ++ (rangedScore ranged Defensive |> String.fromInt))

        -- , separator
        -- , option ("Best Result = " ++ rangedScoreEffect ranged (rangedScore ranged Offensive - rangedScore ranged Defensive + 5))
        ]


viewRangedDefensiveRoll ranged =
    column [ spacing 20 ] <|
        [ title "Defensive Roll"
        , button
            { onPress = Just (RandomRangedDefensiveRoll ranged)
            , label = text "Roll"
            }
        , separator
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetRangedDefensiveRoll ranged i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 1, 2, 3 ]
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetRangedDefensiveRoll ranged i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 4, 5, 6 ]
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        , separator
        , option "Combat = Ranged"
        , option ("Offensive = " ++ baseToString ranged.offensive)
        , option ("Defensive = " ++ baseToString ranged.defensive)
        , separator
        , option ("Offensive Plain Score = " ++ (rangedScore ranged Offensive |> String.fromInt))
        , option ("Defensive Plain Score = " ++ (rangedScore ranged Defensive |> String.fromInt))
        , separator
        , option ("Offensive Roll = " ++ (ranged.offensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        ]


viewCloseOffensiveRoll close =
    column [ spacing 20 ] <|
        [ title "Offensive Roll"
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetCloseOffensiveRoll close i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 1, 2, 3 ]
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetCloseOffensiveRoll close i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 4, 5, 6 ]
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        , separator
        , option "Combat = Ranged"
        , option ("Offensive = " ++ baseToString close.offensive)
        , option ("Defensive = " ++ baseToString close.defensive)
        , separator
        , option ("Offensive Plain Score = " ++ (closeScore close Offensive |> String.fromInt))
        , option ("Defensive Plain Score = " ++ (closeScore close Defensive |> String.fromInt))
        ]


viewCloseDefensiveRoll close =
    column [ spacing 20 ] <|
        [ title "Defensive Roll"
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetCloseDefensiveRoll close i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 1, 2, 3 ]
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetCloseDefensiveRoll close i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 4, 5, 6 ]
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        , separator
        , option "Combat = Ranged"
        , option ("Offensive = " ++ baseToString close.offensive)
        , option ("Defensive = " ++ baseToString close.defensive)
        , separator
        , option ("Offensive Plain Score = " ++ (closeScore close Offensive |> String.fromInt))
        , option ("Defensive Plain Score = " ++ (closeScore close Defensive |> String.fromInt))
        , separator
        , option ("Offensive Roll = " ++ (close.offensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        ]


viewChargingOffensiveRoll ranged =
    column [ spacing 20 ] <|
        [ title "Offensive Roll"
        , button
            { onPress = Just (RandomRangedOffensiveRoll ranged)
            , label = text "Roll"
            }
        , separator
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetRangedOffensiveRoll ranged i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 1, 2, 3 ]
        , row [ spacing 10, width fill ] <|
            List.map
                (\i ->
                    button
                        { onPress = Just (SetRangedOffensiveRoll ranged i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 4, 5, 6 ]
        , separator
        , inactiveButton { onPress = Just Reset, label = text "New" }
        ]


viewRangedScores ranged =
    let
        offensiveScore =
            rangedScore ranged Offensive

        defensiveScore =
            rangedScore ranged Defensive

        offensiveScoreWithoutRoll =
            offensiveScore - Maybe.withDefault 0 ranged.offensiveRoll

        defensiveScoreWithoutRoll =
            defensiveScore - Maybe.withDefault 0 ranged.defensiveRoll

        score =
            offensiveScore - defensiveScore
    in
    column [ spacing 20 ]
        [ if offensiveScore > defensiveScore then
            title "Offensive Wins"

          else if offensiveScore < defensiveScore then
            title "Defensive Wins"

          else
            title "Draw"
        , el
            [ Background.color (rgb 0.3 0.6 0.3)
            , Font.center
            , Font.color (rgb 1 1 1)
            , Font.size 24
            , padding 20
            , width fill
            ]
            (text (rangedScoreEffect ranged score))
        , separator
        , button { onPress = Just (ResetRangedRolls ranged), label = text "Re-roll" }
        , button { onPress = Just Reset, label = text "New" }
        , separator
        , option "Combat = Ranged"
        , option ("Offensive = " ++ baseToString ranged.offensive)
        , option ("Defensive = " ++ baseToString ranged.defensive)
        , separator
        , option ("Offensive Plain Score = " ++ (offensiveScoreWithoutRoll |> String.fromInt))
        , option ("Defensive Plain Score = " ++ (defensiveScoreWithoutRoll |> String.fromInt))
        , separator
        , option ("Offensive Roll = " ++ (ranged.offensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        , option ("Defensive Roll = " ++ (ranged.defensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        , separator
        , option ("Offensive Score = " ++ (offensiveScore |> String.fromInt))
        , option ("Defensive Score = " ++ (defensiveScore |> String.fromInt))
        ]


viewCloseScores close =
    let
        offensiveScore =
            closeScore close Offensive

        defensiveScore =
            closeScore close Defensive

        offensiveScoreWithoutRoll =
            offensiveScore - Maybe.withDefault 0 close.offensiveRoll

        defensiveScoreWithoutRoll =
            defensiveScore - Maybe.withDefault 0 close.defensiveRoll

        score =
            offensiveScore - defensiveScore
    in
    column [ spacing 20 ]
        [ if offensiveScore > defensiveScore then
            title "Offensive Wins"

          else if offensiveScore < defensiveScore then
            title "Defensive Wins"

          else
            title "Draw"
        , el
            [ Background.color (rgb 0.3 0.6 0.3)
            , Font.center
            , Font.color (rgb 1 1 1)
            , Font.size 24
            , padding 20
            , width fill
            ]
            (text (closeScoreEffect close score))
        , separator
        , button { onPress = Just (ResetCloseRolls close), label = text "Re-roll" }
        , button { onPress = Just Reset, label = text "New" }
        , separator
        , option "Combat = Close"
        , option ("Offensive = " ++ baseToString close.offensive)
        , option ("Defensive = " ++ baseToString close.defensive)
        , separator
        , option ("Offensive Plain Score = " ++ (offensiveScoreWithoutRoll |> String.fromInt))
        , option ("Defensive Plain Score = " ++ (defensiveScoreWithoutRoll |> String.fromInt))
        , separator
        , option ("Offensive Roll = " ++ (close.offensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        , option ("Defensive Roll = " ++ (close.defensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        , separator
        , option ("Offensive Score = " ++ (offensiveScore |> String.fromInt))
        , option ("Defensive Score = " ++ (defensiveScore |> String.fromInt))
        ]



--


title t =
    el [ Font.bold, Font.size 32, centerX, paddingEach { top = 0, right = 0, bottom = 30, left = 0 } ] (text t)


option t =
    el [ Font.size 16, centerX ] (text t)


separator =
    el [ Font.size 16, centerX ] (text "--")


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


modifierButton label isActive msg =
    row [ spacing 20, width fill ]
        [ text label
        , el [ alignRight ] <|
            (if isActive then
                button

             else
                inactiveButton
            )
                { onPress = Just msg
                , label = text "✓"
                }
        ]


numberedButton label numbers value msg =
    row [ spacing 20, width fill ]
        [ text label
        , row [ spacing 2, width fill ] <|
            List.map
                (\i ->
                    el [ alignRight ] <|
                        (if value == i then
                            button

                         else
                            inactiveButton
                        )
                            { onPress = Just (msg i)
                            , label = text (String.fromInt i)
                            }
                )
                numbers
        ]


optionButtons label options value msg =
    row [ spacing 20, width fill ]
        [ text label
        , row [ spacing 2, width fill ] <|
            List.map
                (\( v, t ) ->
                    (if value == v then
                        button

                     else
                        inactiveButton
                    )
                        { onPress = Just (msg v)
                        , label = text t
                        }
                )
                options
        ]


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
