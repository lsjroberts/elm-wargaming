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
    | Charge ChargeCombat


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


type alias ChargeCombat =
    { offensive : BaseType
    , defensive : BaseType
    , offensiveModifiers : ChargeModifiers
    , defensiveModifiers : ChargeModifiers
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


allBases =
    [ Shot
    , ShotHeavy
    , Mixed
    , PikeHeavy
    , Pike
    , DismountedDragoons
    , MountedDragoons
    , HorseSwedish
    , HorseDutch
    , Artillery
    ]


closeBases =
    List.filter (\t -> t /= Artillery) allBases


rangedBases =
    List.filter (\t -> t /= HorseSwedish && t /= HorseDutch) allBases


type alias RangedModifiers =
    { longRange : Bool
    , artilleryDistance : Int
    , shaken : Int
    , quality : Quality
    , elite : Bool
    , officerAttached : Bool
    , terrain : Int
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
    , terrain : Int
    , saved : Bool
    }


type alias ChargeModifiers =
    { bases : Int
    , shaken : Int
    , officerAttached : Bool
    , quality : Quality
    , elite : Bool
    , terrain : Int
    , uphill : Bool
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
      -- Charge
    | StartCharge
    | SetChargeDefensive ChargeCombat BaseType
    | SetChargeOffensiveModifiers ChargeCombat ChargeModifiers
    | SetChargeDefensiveModifiers ChargeCombat ChargeModifiers
    | SetChargeOffensiveRoll ChargeCombat Int
    | SetChargeDefensiveRoll ChargeCombat Int
    | ResetChargeRolls ChargeCombat


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
    , terrain = 0
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
    , terrain = 0
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

        cond c score =
            if c then
                score

            else
                0
    in
    baseScore
        + rollScore
        + modifiers.terrain
        + (modifiers.shaken * -1)
        + cond modifiers.longRange -4
        + cond (modifiers.quality == Veteran) 1
        + cond modifiers.elite 1
        + cond modifiers.officerAttached 1
        + cond modifiers.uphill 1
        |> (+)
            (if modifiers.artilleryDistance > 1 then
                modifiers.artilleryDistance * -1

             else
                0
            )
        |> (+)
            (if modifiers.quality == Raw && position == Offensive then
                -1

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
                    if close.offensive == MountedDragoons || close.offensive == HorseSwedish || close.offensive == HorseDutch then
                        case close.defensive of
                            Unknown ->
                                0

                            Shot ->
                                2

                            ShotHeavy ->
                                3

                            Mixed ->
                                4

                            PikeHeavy ->
                                5

                            Pike ->
                                6

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

                    else
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

        cond c score =
            if c then
                score

            else
                0
    in
    baseScore
        + rollScore
        + contactingScore
        - modifiers.flanksOverlapped
        - (modifiers.shaken * 2)
        + modifiers.terrain
        + cond modifiers.recoiled -2
        + cond (modifiers.officerAttached == Just Good) 2
        + cond (modifiers.officerAttached == Just Average) 1
        + cond (modifiers.quality == Raw) -1
        |> (+)
            (if modifiers.firstPhase && (modifiers.quality == Veteran || modifiers.elite) then
                1

             else
                0
            )


chargeScore : ChargeCombat -> Position -> Int
chargeScore charge position =
    let
        modifiers =
            case position of
                Offensive ->
                    charge.offensiveModifiers

                Defensive ->
                    charge.defensiveModifiers

        rollScore =
            case position of
                Offensive ->
                    Maybe.withDefault 0 charge.offensiveRoll

                Defensive ->
                    Maybe.withDefault 0 charge.defensiveRoll

        baseScore =
            case position of
                Offensive ->
                    3

                Defensive ->
                    case charge.defensive of
                        Unknown ->
                            0

                        Shot ->
                            2

                        ShotHeavy ->
                            3

                        Mixed ->
                            4

                        PikeHeavy ->
                            5

                        Pike ->
                            6

                        DismountedDragoons ->
                            1

                        MountedDragoons ->
                            1

                        HorseSwedish ->
                            3

                        HorseDutch ->
                            2

                        Artillery ->
                            1

        cond c score =
            if c then
                score

            else
                0
    in
    baseScore
        + rollScore
        + (modifiers.shaken * -1)
        + cond (modifiers.quality == Veteran) 1
        + cond modifiers.elite 1
        + cond (position == Offensive && modifiers.officerAttached) 1
        + cond (position == Offensive && modifiers.uphill) 1
        + cond (position == Defensive) modifiers.terrain


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


chargeScoreEffect charge score =
    if score < 0 then
        "Chargers remain in place, shaken +1"

    else if score <= 1 then
        "Charge home, shaken +1"

    else if score <= 4 then
        "Charge home"

    else if score == 5 then
        "Charge home, target shaken +1"

    else
        "Charge home, target recoils shaken +1"



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
        StartCharge ->
            ( Charge (ChargeCombat HorseSwedish Unknown (ChargeModifiers 0 0 False Trained False 0 False False) (ChargeModifiers 0 0 False Trained False 0 False False) Nothing Nothing), Cmd.none )

        SetChargeDefensive charge base ->
            ( Charge { charge | defensive = base }, Cmd.none )

        SetChargeOffensiveModifiers charge modifiers ->
            ( Charge { charge | offensiveModifiers = modifiers }, Cmd.none )

        SetChargeDefensiveModifiers charge modifiers ->
            ( Charge { charge | defensiveModifiers = modifiers }, Cmd.none )

        SetChargeOffensiveRoll charge roll ->
            ( Charge { charge | offensiveRoll = Just roll }, Cmd.none )

        SetChargeDefensiveRoll charge roll ->
            ( Charge { charge | defensiveRoll = Just roll }, Cmd.none )

        ResetChargeRolls charge ->
            ( Charge { charge | offensiveRoll = Nothing, defensiveRoll = Nothing }, Cmd.none )


rollD6 : Random.Generator Int
rollD6 =
    Random.int 1 6



-- VIEW


view model =
    Element.layout [ height fill ] <|
        case model of
            New ->
                viewNew

            Ranged ranged ->
                viewRanged ranged

            Close close ->
                viewClose close

            Charge charge ->
                viewCharge charge


viewNew =
    column [ width fill, height fill ]
        [ fillButton [ Border.widthEach { top = 0, right = 0, bottom = 20, left = 0 } ] { onPress = Just StartRanged, label = text "Ranged Combat" }
        , fillButton [ Background.color (rgb 0.7 0.3 0.3), Border.widthEach { top = 0, right = 0, bottom = 20, left = 0 } ]
            { onPress = Just StartClose, label = text "Close Combat" }
        , fillButton [ Background.color (rgb 0.3 0.7 0.3) ]
            { onPress = Just StartCharge, label = text "Charge" }

        -- , button { onPress = Just StartCharging, label = text "Charging" }
        ]


viewRanged ranged =
    if ranged.offensive == Unknown then
        viewBaseButtons ranged SetRangedOffensive "Offensive Base" rangedBases

    else if ranged.defensive == Unknown then
        viewBaseButtons ranged SetRangedDefensive "Defensive Base" allBases

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


viewCharge charge =
    if charge.defensive == Unknown then
        viewBaseButtons charge SetChargeDefensive "Defensive Base" allBases

    else if not charge.offensiveModifiers.saved then
        viewChargeOffensiveModifiers charge

    else if not charge.defensiveModifiers.saved then
        viewChargeDefensiveModifiers charge

    else if charge.offensiveRoll == Nothing then
        viewChargeOffensiveRoll charge

    else if charge.defensiveRoll == Nothing then
        viewChargeDefensiveRoll charge

    else
        viewChargeScores charge


viewClose close =
    if close.offensive == Unknown then
        viewBaseButtons close SetCloseOffensive "Offensive Base" closeBases

    else if close.defensive == Unknown then
        viewBaseButtons close SetCloseDefensive "Defensive Base" allBases

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


viewCloseBattle close =
    let
        baseIcon =
            el [ width (px 80), height (px 40), Background.color (rgb 0.5 0.5 0.5) ] <| none
    in
    column [ spacing 40 ]
        [ row [ spacing 2, moveRight (toFloat close.counts.overlap * 41) ] <| List.map (\i -> baseIcon) (List.range 1 close.counts.offensive)
        , row [ spacing 2 ] <| List.map (\i -> baseIcon) (List.range 1 close.counts.defensive)
        ]


viewBaseButtons combat setMsg label bases =
    let
        baseButton base =
            fillButton []
                { onPress = Just (setMsg combat base)
                , label = text (baseToString base)
                }
    in
    column [ width fill, height fill ] <|
        [ title label ]
            ++ List.map baseButton bases
            ++ [ resetButton ]


viewRangedOffensiveModifiers ranged =
    let
        modifierButton_ label isActive action =
            modifierButton label (isActive ranged.offensiveModifiers) (SetRangedOffensiveModifiers ranged (action ranged.offensiveModifiers))

        numberedButton_ label numbers get action =
            numberedButton label numbers (get ranged.offensiveModifiers) (\i -> SetRangedOffensiveModifiers ranged (action ranged.offensiveModifiers i))

        optionButtons_ label options get action =
            optionButtons label options (get ranged.offensiveModifiers) (\v -> SetRangedOffensiveModifiers ranged (action ranged.offensiveModifiers v))
    in
    column [ width fill, height fill ]
        [ title "Offensive Modifiers"
        , if ranged.offensive == Artillery then
            numberedButton_ "Artillery Distance" [ 0, 1, 2, 3, 4 ] .artilleryDistance (\m i -> { m | artilleryDistance = i })

          else
            modifierButton_ "Long Range" .longRange (\m -> { m | longRange = not m.longRange })
        , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , optionButtons_ "Level" [ ( Raw, "Raw" ), ( Trained, "Exp" ), ( Veteran, "Vet" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , modifierButton_ "Officer Attached" .officerAttached (\m -> { m | officerAttached = not m.officerAttached })
        , modifierButton_ "Uphill of target" .uphill (\m -> { m | uphill = not m.uphill })
        , nextButton { onPress = Just (SetRangedOffensiveModifiers ranged (ranged.offensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , resetButton
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
    column [ width fill, height fill ]
        [ title "Defensive Modifiers"
        , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , optionButtons_ "Level" [ ( Raw, "Raw" ), ( Trained, "Exp" ), ( Veteran, "Vet" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , numberedButton_ "Terrain" [ 0, 1, 2 ] .terrain (\m i -> { m | terrain = i })
        , nextButton { onPress = Just (SetRangedDefensiveModifiers ranged (ranged.defensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , resetButton
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
    column [ width fill, height fill ]
        [ title "Offensive Modifiers"
        , modifierButton_ "First Phase"
            .firstPhase
            (\m ->
                { m
                    | firstPhase = not m.firstPhase
                    , recoiled =
                        if not m.firstPhase then
                            False

                        else
                            m.recoiled
                }
            )
        , optionButtons_ "Contact" [ ( Static, "No" ), ( ContactingFront, "Ft" ), ( ContactingFlankOrRear, "Fk / R" ) ] .contacting (\m v -> { m | contacting = v })
        , optionButtons_ "Charge" [ ( Static, "No" ), ( ChargingFront, "Ft" ), ( ChargingFlankOrRear, "Fk / R" ) ] .contacting (\m v -> { m | contacting = v })
        , numberedButton_ "Flanks" [ 0, 1, 2 ] .flanksOverlapped (\m i -> { m | flanksOverlapped = i })
        , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , modifierButton_ "Recoiled"
            .recoiled
            (\m ->
                { m
                    | recoiled = not m.recoiled
                    , firstPhase =
                        if not m.recoiled then
                            False

                        else
                            m.firstPhase
                }
            )
        , optionButtons_ "Level" [ ( Raw, "Raw" ), ( Trained, "Exp" ), ( Veteran, "Vet" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , optionButtons_ "Officer" [ ( Nothing, "No" ), ( Just Bad, "7" ), ( Just Average, "8" ), ( Just Good, "9" ) ] .officerAttached (\m v -> { m | officerAttached = v })
        , fillButton [] { onPress = Just (SetCloseOffensiveModifiers close (close.offensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , resetButton
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
    column [ width fill, height fill ]
        [ title "Defensive Modifiers"
        , numberedButton_ "Flanks" [ 0, 1, 2 ] .flanksOverlapped (\m i -> { m | flanksOverlapped = i })
        , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , modifierButton_ "Recoiled" .recoiled (\m -> { m | recoiled = not m.recoiled })
        , optionButtons_ "Level" [ ( Raw, "Raw" ), ( Trained, "Exp" ), ( Veteran, "Vet" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , optionButtons_ "Officer" [ ( Nothing, "No" ), ( Just Bad, "7" ), ( Just Average, "8" ), ( Just Good, "9" ) ] .officerAttached (\m v -> { m | officerAttached = v })
        , numberedButton_ "Terrain" [ 0, 1, 2 ] .terrain (\m i -> { m | terrain = i })
        , fillButton [] { onPress = Just (SetCloseDefensiveModifiers close (close.defensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , resetButton
        ]


viewChargeOffensiveModifiers charge =
    let
        modifierButton_ label isActive action =
            modifierButton label (isActive charge.offensiveModifiers) (SetChargeOffensiveModifiers charge (action charge.offensiveModifiers))

        numberedButton_ label numbers get action =
            numberedButton label numbers (get charge.offensiveModifiers) (\i -> SetChargeOffensiveModifiers charge (action charge.offensiveModifiers i))

        optionButtons_ label options get action =
            optionButtons label options (get charge.offensiveModifiers) (\v -> SetChargeOffensiveModifiers charge (action charge.offensiveModifiers v))
    in
    column [ width fill, height fill ]
        [ title "Offensive Modifiers"
        , numberedButton_ "Extra Bases" [ 0, 1, 2 ] .bases (\m i -> { m | bases = i })

        -- , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , optionButtons_ "Level" [ ( Raw, "Raw" ), ( Trained, "Exp" ), ( Veteran, "Vet" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , modifierButton_ "Officer Attached" .officerAttached (\m -> { m | officerAttached = not m.officerAttached })
        , modifierButton_ "Uphill" .uphill (\m -> { m | uphill = not m.uphill })
        , nextButton { onPress = Just (SetChargeOffensiveModifiers charge (charge.offensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , resetButton
        ]


viewChargeDefensiveModifiers charge =
    let
        modifierButton_ label isActive action =
            modifierButton label (isActive charge.defensiveModifiers) (SetChargeDefensiveModifiers charge (action charge.defensiveModifiers))

        numberedButton_ label numbers get action =
            numberedButton label numbers (get charge.defensiveModifiers) (\i -> SetChargeDefensiveModifiers charge (action charge.defensiveModifiers i))

        optionButtons_ label options get action =
            optionButtons label options (get charge.defensiveModifiers) (\v -> SetChargeDefensiveModifiers charge (action charge.defensiveModifiers v))
    in
    column [ width fill, height fill ]
        [ title "Defensive Modifiers"
        , numberedButton_ "Shaken" [ 0, 1, 2 ] .shaken (\m i -> { m | shaken = i })
        , optionButtons_ "Level" [ ( Raw, "Raw" ), ( Trained, "Exp" ), ( Veteran, "Vet" ) ] .quality (\m v -> { m | quality = v })
        , modifierButton_ "Elite" .elite (\m -> { m | elite = not m.elite })
        , numberedButton_ "Terrain" [ 0, 1, 2 ] .terrain (\m i -> { m | terrain = i })
        , nextButton { onPress = Just (SetChargeDefensiveModifiers charge (charge.defensiveModifiers |> (\m -> { m | saved = True }))), label = text "Next →" }
        , resetButton
        ]


viewRangedOffensiveRoll ranged =
    viewRoll "Offensive Roll" ranged SetRangedOffensiveRoll rangedScore


viewRangedDefensiveRoll ranged =
    viewRoll "Defensive Roll" ranged SetRangedDefensiveRoll rangedScore


viewCloseOffensiveRoll close =
    viewRoll "Offensive Roll" close SetCloseOffensiveRoll closeScore


viewCloseDefensiveRoll close =
    viewRoll "Defensive Roll" close SetCloseDefensiveRoll closeScore


viewChargeOffensiveRoll charge =
    viewRoll "Offensive Roll" charge SetChargeOffensiveRoll chargeScore


viewChargeDefensiveRoll charge =
    viewRoll "Defensive Roll" charge SetChargeDefensiveRoll chargeScore


viewRoll label model setRollMsg scorer =
    column [ width fill, height fill ] <|
        [ title label
        , titleWith
            (column [ width fill, spacing 20 ]
                [ el [ centerX ] <|
                    text <|
                        baseToString model.offensive
                            ++ " ("
                            ++ (scorer model Offensive |> String.fromInt)
                            ++ ")"
                , el [ centerX ] <| text <| "vs."
                , el [ centerX ] <|
                    text <|
                        baseToString model.defensive
                            ++ " ("
                            ++ (scorer model Defensive |> String.fromInt)
                            ++ ")"
                ]
            )
        , column [ width fill, height fill ] <|
            List.map
                (\i ->
                    fillButton []
                        { onPress = Just (setRollMsg model i)
                        , label = text (String.fromInt i)
                        }
                )
                [ 1, 2, 3, 4, 5, 6 ]
        , resetButton
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
    column [ width fill, height fill ]
        [ if offensiveScore > defensiveScore then
            title "Offensive Wins"

          else if offensiveScore < defensiveScore then
            title "Defensive Wins"

          else
            title "Draw"
        , fillWith
            [ Background.color (rgb 0.3 0.6 0.3), Font.color (rgb 1 1 1) ]
            (paragraph [ centerX, centerY ] [ text (rangedScoreEffect ranged score) ])
        , titleWith
            (column [ width fill, spacing 20 ]
                [ el [ centerX ] <|
                    text <|
                        baseToString ranged.offensive
                            ++ " ("
                            ++ (offensiveScore |> String.fromInt)
                            ++ ")"
                , el [ centerX ] <| text <| "vs."
                , el [ centerX ] <|
                    text <|
                        baseToString ranged.defensive
                            ++ " ("
                            ++ (defensiveScore |> String.fromInt)
                            ++ ")"
                ]
            )
        , fillButton []
            { onPress = Just (ResetRangedRolls ranged), label = text "Re-roll" }
        , resetButton

        -- , separator
        -- , option "Combat = Ranged"
        -- , option ("Offensive = " ++ baseToString ranged.offensive)
        -- , option ("Defensive = " ++ baseToString ranged.defensive)
        -- , separator
        -- , option ("Offensive Plain Score = " ++ (offensiveScoreWithoutRoll |> String.fromInt))
        -- , option ("Defensive Plain Score = " ++ (defensiveScoreWithoutRoll |> String.fromInt))
        -- , separator
        -- , option ("Offensive Roll = " ++ (ranged.offensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        -- , option ("Defensive Roll = " ++ (ranged.defensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        -- , separator
        -- , option ("Offensive Score = " ++ (offensiveScore |> String.fromInt))
        -- , option ("Defensive Score = " ++ (defensiveScore |> String.fromInt))
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
    column [ width fill, height fill ]
        [ if offensiveScore > defensiveScore then
            title "Offensive Wins"

          else if offensiveScore < defensiveScore then
            title "Defensive Wins"

          else
            title "Draw"
        , fillWith
            [ Background.color (rgb 0.3 0.6 0.3), Font.color (rgb 1 1 1) ]
            (paragraph [ centerX, centerY ] [ text (closeScoreEffect close score) ])
        , titleWith
            (column [ width fill, spacing 20 ]
                [ el [ centerX ] <|
                    text <|
                        baseToString close.offensive
                            ++ " ("
                            ++ (offensiveScore |> String.fromInt)
                            ++ ")"
                , el [ centerX ] <| text <| "vs."
                , el [ centerX ] <|
                    text <|
                        baseToString close.defensive
                            ++ " ("
                            ++ (defensiveScore |> String.fromInt)
                            ++ ")"
                ]
            )
        , fillButton []
            { onPress = Just (ResetCloseRolls close), label = text "Re-roll" }
        , resetButton

        -- , separator
        -- , option "Combat = Close"
        -- , option ("Offensive = " ++ baseToString close.offensive)
        -- , option ("Defensive = " ++ baseToString close.defensive)
        -- , separator
        -- , option ("Offensive Plain Score = " ++ (offensiveScoreWithoutRoll |> String.fromInt))
        -- , option ("Defensive Plain Score = " ++ (defensiveScoreWithoutRoll |> String.fromInt))
        -- , separator
        -- , option ("Offensive Roll = " ++ (close.offensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        -- , option ("Defensive Roll = " ++ (close.defensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        -- , separator
        -- , option ("Offensive Score = " ++ (offensiveScore |> String.fromInt))
        -- , option ("Defensive Score = " ++ (defensiveScore |> String.fromInt))
        ]


viewChargeScores charge =
    let
        offensiveScore =
            chargeScore charge Offensive

        defensiveScore =
            chargeScore charge Defensive

        offensiveScoreWithoutRoll =
            offensiveScore - Maybe.withDefault 0 charge.offensiveRoll

        defensiveScoreWithoutRoll =
            defensiveScore - Maybe.withDefault 0 charge.defensiveRoll

        score =
            offensiveScore - defensiveScore
    in
    column [ width fill, height fill ]
        [ if offensiveScore > defensiveScore then
            title "Offensive Wins"

          else if offensiveScore < defensiveScore then
            title "Defensive Wins"

          else
            title "Draw"
        , fillWith
            [ Background.color (rgb 0.3 0.6 0.3), Font.color (rgb 1 1 1) ]
            (paragraph [ centerX, centerY ] [ text (chargeScoreEffect charge score) ])
        , titleWith
            (column [ width fill, spacing 20 ]
                [ el [ centerX ] <|
                    text <|
                        baseToString charge.offensive
                            ++ " ("
                            ++ (offensiveScore |> String.fromInt)
                            ++ ")"
                , el [ centerX ] <| text <| "vs."
                , el [ centerX ] <|
                    text <|
                        baseToString charge.defensive
                            ++ " ("
                            ++ (defensiveScore |> String.fromInt)
                            ++ ")"
                ]
            )
        , fillButton []
            { onPress = Just (ResetChargeRolls charge), label = text "Re-roll" }
        , resetButton

        -- , separator
        -- , option "Combat = Ranged"
        -- , option ("Offensive = " ++ baseToString ranged.offensive)
        -- , option ("Defensive = " ++ baseToString ranged.defensive)
        -- , separator
        -- , option ("Offensive Plain Score = " ++ (offensiveScoreWithoutRoll |> String.fromInt))
        -- , option ("Defensive Plain Score = " ++ (defensiveScoreWithoutRoll |> String.fromInt))
        -- , separator
        -- , option ("Offensive Roll = " ++ (ranged.offensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        -- , option ("Defensive Roll = " ++ (ranged.defensiveRoll |> Maybe.withDefault 0 |> String.fromInt))
        -- , separator
        -- , option ("Offensive Score = " ++ (offensiveScore |> String.fromInt))
        -- , option ("Defensive Score = " ++ (defensiveScore |> String.fromInt))
        ]



--


titleWith e =
    el
        [ Background.color (rgb 0.7 0.3 0.7)
        , Border.widthEach { top = 0, right = 0, bottom = 20, left = 0 }
        , Border.color (rgb 1 1 1)
        , Font.color (rgb 1 1 1)
        , Font.bold
        , Font.size 60
        , Font.center
        , padding 30
        , width fill
        ]
        e


title t =
    titleWith
        (text t)


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
        , padding 20
        , width fill
        ]


fillWith styles e =
    el
        ([ Font.center
         , Font.size 60
         , paddingXY 60 10
         , width fill
         , height fill
         ]
            ++ styles
        )
        e


fillButton styles =
    Input.button
        ([ Background.color (rgb 0.3 0.3 0.7)
         , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
         , Border.color (rgb 1 1 1)
         , Font.center
         , Font.color (rgb 1 1 1)
         , Font.size 60
         , paddingXY 60 10
         , width fill
         , height fill
         ]
            ++ styles
        )


resetButton =
    fillButton
        [ Background.color (rgb 0.7 0.7 0.7)
        , Border.widthEach { top = 20, right = 0, bottom = 0, left = 0 }
        , Border.color (rgb 1 1 1)
        ]
        { onPress = Just Reset, label = text "New" }


nextButton =
    fillButton
        [ Background.color (rgb 0.7 0.3 0.7)
        , Border.widthEach { top = 20, right = 0, bottom = 0, left = 0 }
        , Border.color (rgb 1 1 1)
        ]


inactiveFillButton =
    fillButton [ Background.color (rgb 0.7 0.7 0.7) ]


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
    (if isActive then
        fillButton []

     else
        inactiveFillButton
    )
        { onPress = Just msg
        , label =
            row [ width fill ]
                [ text label
                , el [ alignRight ]
                    (if isActive then
                        text "✓"

                     else
                        text "✕"
                    )
                ]
        }


numberedButton label numbers value msg =
    row [ width fill, height fill ]
        [ el
            [ Background.color (rgb 0.7 0.6 0.7)
            , Font.center
            , Font.color (rgb 1 1 1)
            , Font.size 60
            , paddingXY 60 10
            , width fill
            , height fill
            ]
            (el [ centerY ] (text label))
        , row [ width fill, height fill ] <|
            List.map
                (\i ->
                    (if value == i then
                        fillButton []

                     else
                        inactiveFillButton
                    )
                        { onPress = Just (msg i)
                        , label = text (String.fromInt i)
                        }
                )
                numbers
        ]


optionButtons label options value msg =
    row [ width fill, height fill ]
        [ el
            [ Background.color (rgb 0.7 0.6 0.7)
            , Font.center
            , Font.color (rgb 1 1 1)
            , Font.size 60
            , paddingXY 60 10
            , width fill
            , height fill
            ]
            (el [ centerY ] (text label))
        , row [ width fill, height fill ] <|
            List.map
                (\( v, t ) ->
                    (if value == v then
                        fillButton []

                     else
                        inactiveFillButton
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
