port module Main exposing (main)

import Browser
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List exposing (..)
import Random exposing (..)
import List.Extra exposing (getAt)
import Tuple
import Basics exposing (..)
import Task
import Time exposing (..)
import Process

-- Typ-Deklarationen

type alias Seat = 
    { name : String 
    , hidden : Bool
    , modal : Bool
    , id : Int
    , nextText : String --Für Dialoge
    , spokenText : String
    , index : Int -- Für Dialoge
    }

type alias RandomValues =
    { randomIndex : Int
    , randomSeat : Int
    }

type alias Model =
    { width : Int
    , height : Int
    , hidden : Bool 
    , seat1 : Seat
    , seat2 : Seat
    , seat3 : Seat
    , seat4 : Seat
    , seat5 : Seat
    , nextSeat : Int
    , person_list : List String -- Liste an möglichen Gäste
    , seat_list : List String --Liste an Stühlen
    , randomString : Maybe String -- Next Guest
    , showModal : Bool
    , userInput : String
    , timeChoosen : Bool
    , time : Int
    }

--Initialisierung

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 
    10 
    10 
    True 
    {name = "Random_Person.png", hidden = True, modal = False, id = 0, nextText = "Next Text", spokenText = "", index = 0} 
    {name = "Random_Person.png", hidden = True, modal = False, id = 1, nextText = "Next Text", spokenText = "", index = 0} 
    {name = "Random_Person.png", hidden = True, modal = False, id = 2, nextText = "Next Text", spokenText = "", index = 0} 
    {name = "Random_Person.png", hidden = True, modal = False, id = 3, nextText = "Next Text", spokenText = "", index = 0} 
    {name = "Random_Person.png", hidden = True, modal = False, id = 4, nextText = "Next Text", spokenText = "", index = 0} 
    0 
    ["Person1.png","Person2.png","Person3.png","Person4.png"] 
    ["0","1","2","3","4"] 
    Nothing 
    False
    "..."
    False
    0
    , Cmd.none )

-- Message Typen

type Msg
    = WindowResized (List Int)
    | PrepNextNPC
    | NPCClicked Seat
    | GotRandomValues RandomValues
    | Tick Posix --Dialogfenster, dadurch wird Dialog nach und nach angezeigt
    | RemoveNPC Seat
    | GetInput String
    | TickMinute Posix
    | SwitchOverlay

--Hilfsfunktionen
checkForInt: String -> Bool
checkForInt word =
    case String.toInt word of 
        Just a -> 
            True
        Nothing ->
            False

removeWord : Maybe String -> List String -> List String --Entfernt Eintrag aus einer Liste
removeWord word liste = 
    case word of 
        Just a ->
            List.filter ((/=) a) liste
        Nothing ->
            liste

generateRandomValues : Int -> Int -> Generator RandomValues
generateRandomValues listPeople listSeats =
    Random.map2 RandomValues
        (Random.int 0 (listPeople - 1))
        (Random.int 0 (listSeats - 1))



--Update Funktionen

update : Msg -> Model -> ( Model, Cmd Msg ) --Es darf erst der nächste NPC gepreppt werden, wenn der erste abgearbeitet wurde
update msg model =
    case msg of
        WindowResized liste ->
            case (head liste, head (reverse liste)) of 
                (Just a, Just b) ->
                    ( { model | width = a, height = b }, Cmd.none )
                (Just a, Nothing) -> 
                    ( { model | width = a, height = 0 }, Cmd.none )
                (Nothing, Just b) ->
                    ( { model | width = 0, height = b }, Cmd.none )
                (Nothing, Nothing) ->    
                    ( { model | width = 0, height = 0 }, Cmd.none )

        PrepNextNPC -> --Random Sitzplatz und Random person ermitteln
            let
                listPeople = List.length model.person_list
                listSeats = List.length model.seat_list
                randomValuesGenerator = generateRandomValues listPeople listSeats
            in
            ( model, Random.generate GotRandomValues randomValuesGenerator)


        NPCClicked seat ->   --Echte Person zeigen

                if seat.name == "Random_Person.png" then --Person wird zum enthülle angeklickt
                    case seat.id of 
                        0 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat1 = {name = a, hidden = False, modal = False, id = model.seat1.id, nextText = model.seat1.nextText, spokenText = model.seat1.spokenText, index = model.seat1.index} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat1 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat1.id, nextText = model.seat1.nextText, spokenText = model.seat1.spokenText, index = model.seat1.index} }, Cmd.none )
                        1 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat2 = {name = a, hidden = False, modal = False, id = model.seat2.id, nextText = model.seat2.nextText, spokenText = model.seat2.spokenText, index = model.seat2.index} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat2 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat2.id, nextText = model.seat2.nextText, spokenText = model.seat2.spokenText, index = model.seat2.index} }, Cmd.none )
                        2 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat3 = {name = a, hidden = False, modal = False, id = model.seat3.id, nextText = model.seat3.nextText, spokenText = model.seat3.spokenText, index = model.seat3.index} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat3 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat3.id, nextText = model.seat3.nextText, spokenText = model.seat3.spokenText, index = model.seat3.index} }, Cmd.none )
                        3 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat4 = {name = a, hidden = False, modal = False, id = model.seat4.id, nextText = model.seat4.nextText, spokenText = model.seat4.spokenText, index = model.seat4.index} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat4 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat4.id, nextText = model.seat4.nextText, spokenText = model.seat4.spokenText, index = model.seat4.index} }, Cmd.none )
                        4 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat5 = {name = a, hidden = False, modal = False, id = model.seat5.id, nextText = model.seat5.nextText, spokenText = model.seat5.spokenText, index = model.seat5.index} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat5 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat5.id, nextText = model.seat5.nextText, spokenText = model.seat5.spokenText, index = model.seat5.index} }, Cmd.none )
                        _ -> 
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat1 = {name = a, hidden = False, modal = False, id = model.seat1.id, nextText = model.seat1.nextText, spokenText = model.seat1.spokenText, index = model.seat1.index} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat1 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat1.id, nextText = model.seat1.nextText, spokenText = model.seat1.spokenText, index = model.seat1.index} }, Cmd.none )

                    else --Person wird zum sprechen angeklickt

                         case seat.id of 
                        0 ->
                                ( { model | seat1 = {name = model.seat1.name, hidden = model.seat1.hidden, modal = not model.seat1.modal, id = model.seat1.id, nextText = model.seat1.nextText, spokenText = "", index = 0}}, Cmd.none )
                        1 ->
                                ( { model | seat2 = {name = model.seat2.name, hidden = model.seat2.hidden, modal = not model.seat2.modal, id = model.seat2.id, nextText = model.seat2.nextText, spokenText = "", index = 0}}, Cmd.none )
                        2 ->
                                ( { model | seat3 = {name = model.seat3.name, hidden = model.seat3.hidden, modal = not model.seat3.modal, id = model.seat3.id, nextText = model.seat3.nextText, spokenText = "", index = 0}}, Cmd.none )
                                
                        3 ->
                                ( { model | seat4 = {name = model.seat4.name, hidden = model.seat4.hidden, modal = not model.seat4.modal, id = model.seat4.id, nextText = model.seat4.nextText, spokenText = "", index = 0}}, Cmd.none )
                                
                        4 ->
                                ( { model | seat5 = {name = model.seat5.name, hidden = model.seat5.hidden, modal = not model.seat5.modal, id = model.seat5.id, nextText = model.seat5.nextText, spokenText = "", index = 0}}, Cmd.none )
                                
                        _ -> 
                                ( { model | seat1 = {name = model.seat1.name, hidden = model.seat1.hidden, modal = not model.seat1.modal, id = model.seat1.id, nextText = model.seat1.nextText, spokenText = "", index = 0}}, Cmd.none )

        GotRandomValues randomValues ->
            let
                randomStr = List.Extra.getAt randomValues.randomIndex model.person_list
                randomSeat = List.Extra.getAt randomValues.randomSeat model.seat_list
            in
            case randomStr of 
                Just b ->
                    case randomSeat of 
                        Just a -> 
                            if a == "0" then
                                    ( { model | seat1 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat1.id, nextText = model.seat1.nextText, spokenText = model.seat1.spokenText, index = model.seat1.index}, randomString = randomStr, nextSeat = 0, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..."}, Cmd.none )
                            else if a == "1" then
                                    ( { model | seat2 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat2.id, nextText = model.seat2.nextText, spokenText = model.seat2.spokenText, index = model.seat2.index}, randomString = randomStr, nextSeat = 1, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                            else if a == "2" then
                                    ( { model | seat3 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat3.id, nextText = model.seat3.nextText, spokenText = model.seat3.spokenText, index = model.seat3.index}, randomString = randomStr, nextSeat = 2, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                            else if a == "3" then
                                    ( { model | seat4 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat4.id, nextText = model.seat4.nextText, spokenText = model.seat4.spokenText, index = model.seat4.index}, randomString = randomStr, nextSeat = 3, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                            else if a == "4" then
                                    ( { model | seat5 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat5.id, nextText = model.seat5.nextText, spokenText = model.seat5.spokenText, index = model.seat5.index}, randomString = randomStr, nextSeat = 4, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                            else
                                    ( { model | seat1 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat1.id, nextText = model.seat1.nextText, spokenText = model.seat1.spokenText, index = model.seat1.index}, randomString = randomStr, nextSeat = 0, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                        Nothing ->
                            ( { model | randomString = randomStr, nextSeat = 1, person_list = removeWord randomStr model.person_list, timeChoosen = not model.timeChoosen, userInput = "..."}, Cmd.none )
                Nothing ->
                    ( { model | randomString = randomStr, nextSeat = 1, timeChoosen = not model.timeChoosen, userInput = "..."}, Cmd.none )
                    
        Tick newTime ->
            if model.seat1.modal == True then
                let
                    newIndex = model.seat1.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat1.nextText
                in
                if newIndex <= String.length model.seat1.nextText then
                    ( { model | seat1 = {name = model.seat1.name, hidden = model.seat1.hidden, modal = model.seat1.modal, id = model.seat1.id, nextText = model.seat1.nextText, spokenText = newDisplayedText, index = newIndex}}, Cmd.none)
                else
                    ( model, Cmd.none )
            else if model.seat2.modal == True then
                let
                    newIndex = model.seat2.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat2.nextText
                in
                if newIndex <= String.length model.seat2.nextText then
                    ( { model | seat2 = {name = model.seat2.name, hidden = model.seat2.hidden, modal = model.seat2.modal, id = model.seat2.id, nextText = model.seat2.nextText, spokenText = newDisplayedText, index = newIndex}}, Cmd.none)
                else
                    ( model, Cmd.none )
            else if model.seat3.modal == True then
                let
                    newIndex = model.seat3.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat3.nextText
                in
                if newIndex <= String.length model.seat3.nextText then
                    ( { model | seat3 = {name = model.seat3.name, hidden = model.seat3.hidden, modal = model.seat3.modal, id = model.seat3.id, nextText = model.seat3.nextText, spokenText = newDisplayedText, index = newIndex}}, Cmd.none)
                else
                    ( model, Cmd.none )
            else if model.seat4.modal == True then
                let
                    newIndex = model.seat4.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat4.nextText
                in
                if newIndex <= String.length model.seat4.nextText then
                    ( { model | seat4 = {name = model.seat4.name, hidden = model.seat4.hidden, modal = model.seat4.modal, id = model.seat4.id, nextText = model.seat4.nextText, spokenText = newDisplayedText, index = newIndex}}, Cmd.none)
                else
                    ( model, Cmd.none )
            else if model.seat5.modal == True then
                let
                    newIndex = model.seat5.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat5.nextText
                in
                if newIndex <= String.length model.seat5.nextText then
                    ( { model | seat5 = {name = model.seat5.name, hidden = model.seat5.hidden, modal = model.seat5.modal, id = model.seat5.id, nextText = model.seat5.nextText, spokenText = newDisplayedText, index = newIndex}}, Cmd.none)
                else
                    ( model, Cmd.none )
            else 
                ( model, Cmd.none )

        RemoveNPC seat -> 
            case seat.id of 
                        0 ->
                                ( { model | person_list = append model.person_list [seat.name],seat_list = append model.seat_list [String.fromInt seat.id],seat1 = {name = "Random_Person.png", hidden = True, modal = False, id = 0, nextText = "Next Text", spokenText = "", index = 0}}, Cmd.none )
                        1 ->
                                ( { model | person_list = append model.person_list [seat.name],seat_list = append model.seat_list [String.fromInt seat.id],seat2 = {name = "Random_Person.png", hidden = True, modal = False, id = 1, nextText = "Next Text", spokenText = "", index = 0}}, Cmd.none )
                        2 ->
                                ( { model | person_list = append model.person_list [seat.name],seat_list = append model.seat_list [String.fromInt seat.id],seat3 = {name = "Random_Person.png", hidden = True, modal = False, id = 2, nextText = "Next Text", spokenText = "", index = 0}}, Cmd.none )
                        3 ->
                                ( { model | person_list = append model.person_list [seat.name],seat_list = append model.seat_list [String.fromInt seat.id],seat4 = {name = "Random_Person.png", hidden = True, modal = False, id = 3, nextText = "Next Text", spokenText = "", index = 0}}, Cmd.none )
                        4 ->
                                ( { model | person_list = append model.person_list [seat.name],seat_list = append model.seat_list [String.fromInt seat.id],seat5 = {name = "Random_Person.png", hidden = True, modal = False, id = 4, nextText = "Next Text", spokenText = "", index = 0}}, Cmd.none )
                        _ -> 
                                ( { model | person_list = append model.person_list [seat.name],seat_list = append model.seat_list [String.fromInt seat.id],seat1 = {name = "Random_Person.png", hidden = True, modal = False, id = 0, nextText = "Next Text", spokenText = "", index = 0}}, Cmd.none )
        
        GetInput input ->
            ({model | userInput = input}, Cmd.none )

        TickMinute _ ->
            let
                    newTime = model.time - 1
            in
            if model.timeChoosen && newTime /= 0 then
                ({ model | time = model.time - 1 }, Cmd.none )
            else if model.timeChoosen && newTime == 0 then
                update PrepNextNPC model
            else 
                (model, Cmd.none)

        SwitchOverlay ->
            if model.timeChoosen then
                update PrepNextNPC model
            else
                case String.toInt model.userInput of 
                    Just a ->
                        ({ model | timeChoosen = not model.timeChoosen, time = a, userInput = "..." }, Cmd.none )
                    Nothing ->
                        (model, Cmd.none )

-- SUBSCRIPTIONS

port windowSize : (List Int -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [Time.every 50 Tick --Gespräche
    ,Time.every (60 * 1000) TickMinute --Minuten Timer
    ,windowSize WindowResized --Bildschirmgröße
    ]

-- VIEW

view : Model -> Html Msg
view model =
    div [ style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        ]
        [ img
            [ src "Theke.gif"
            , style "height" <| String.fromInt model.height ++ "px" 
            , style "width" <| String.fromInt model.width ++ "px" 
            ]
            []
        , button --Seat1
            [ Html.Events.onClick (NPCClicked model.seat1)
            , if model.seat1.hidden == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "left" "1.4%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (model.seat1.name)
                , if model.seat1.hidden == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , if model.seat1.modal then --Seat1 Talking Scene
                div [ style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "width" "100%"
                , style "height" "100%"
                , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "zIndex" "2"
            ]
                [ div
                    [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , style "position" "absolute"
                    , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                    , style "left" "1.4%"  -- 0.5% vom rechten Rand
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat1)
                        , style "width" "100%"
                        , style "height" "100%"
                        , style "zIndex" "1"
                        , style "background" "none"
                        , style "border" "none"
                        , style "padding" "0"
                        , style "cursor" "pointer"
                        , style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat1.name)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [ style "position" "absolute"
                    , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , style "bottom" "50%"
                    , style "left" "1.4%"  -- 0.5% vom rechten Rand
                    , style "color" "white"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"
                    , style "padding" "10px"
                    , style "border-radius" "5px"
                    , style "zIndex" "1"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    , style "justify-content" "space-between"
                    ]
                    [ div [] [ text model.seat1.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat1)
                        , style "width" "50%"
                        , style "height" "10%"
                        , style "zIndex" "1"
                        , style "cursor" "pointer"
                        , style "top" "90%"
                        , style "left" "0%"
                        ] 
                        [ text "Gehen sie bitte!" ]
                    ]
                ]
                  else
                    text ""
        , button --Seat2
            [ Html.Events.onClick (NPCClicked model.seat2)
            , if (model.seat2.hidden) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "left" "21.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (model.seat2.name)
                , if (model.seat2.hidden) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
       , if model.seat2.modal then --Seat2 Talking Scene
                div [ style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "width" "100%"
                , style "height" "100%"
                , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "zIndex" "2"
            ]
                [ div
                    [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , style "position" "absolute"
                    , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                    , style "left" "21.5%"  -- 0.5% vom rechten Rand
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat2)
                        , style "width" "100%"
                        , style "height" "100%"
                        , style "zIndex" "1"
                        , style "background" "none"
                        , style "border" "none"
                        , style "padding" "0"
                        , style "cursor" "pointer"
                        , style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat2.name)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [ style "position" "absolute"
                    , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , style "bottom" "50%"
                    , style "left" "21.5%"  -- 0.5% vom rechten Rand
                    , style "color" "white"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"
                    , style "padding" "10px"
                    , style "border-radius" "5px"
                    , style "zIndex" "1"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    , style "justify-content" "space-between"
                    ]
                    [ div [] [ text model.seat2.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat2)
                        , style "width" "50%"
                        , style "height" "10%"
                        , style "zIndex" "1"
                        , style "cursor" "pointer"
                        , style "top" "90%"
                        , style "left" "0%"
                        ] 
                        [ text "Gehen sie bitte!" ]
                    ]
                ]
                  else
                    text ""
        , button --Seat3
            [ Html.Events.onClick (NPCClicked model.seat3)
            , if (model.seat3.hidden) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "right" "41.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (model.seat3.name)
                , if (model.seat3.hidden) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , if model.seat3.modal then --Seat3 Talking Scene
                div [ style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "width" "100%"
                , style "height" "100%"
                , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "zIndex" "2"
            ]
                [ div
                    [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , style "position" "absolute"
                    , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                    , style "right" "41.5%"  -- 0.5% vom rechten Rand
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat3)
                        , style "width" "100%"
                        , style "height" "100%"
                        , style "zIndex" "1"
                        , style "background" "none"
                        , style "border" "none"
                        , style "padding" "0"
                        , style "cursor" "pointer"
                        , style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat3.name)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [ style "position" "absolute"
                    , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , style "bottom" "50%"
                    , style "right" "41.5%"  -- 0.5% vom rechten Rand
                    , style "color" "white"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"
                    , style "padding" "10px"
                    , style "border-radius" "5px"
                    , style "zIndex" "1"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    , style "justify-content" "space-between"
                    ]
                    [ div [] [ text model.seat3.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat3)
                        , style "width" "50%"
                        , style "height" "10%"
                        , style "zIndex" "1"
                        , style "cursor" "pointer"
                        , style "top" "90%"
                        , style "left" "0%"
                        ] 
                        [ text "Gehen sie bitte!" ]
                    ]
                ]
                  else
                    text ""
        , button --Seat4
            [ Html.Events.onClick (NPCClicked model.seat4)
            , if (model.seat4.hidden) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "right" "21.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (model.seat4.name)
                , if (model.seat4.hidden) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
         , if model.seat4.modal then --Seat4 Talking Scene
                div [ style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "width" "100%"
                , style "height" "100%"
                , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "zIndex" "2"
            ]
                [ div
                    [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , style "position" "absolute"
                    , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                    , style "right" "21.5%"  -- 0.5% vom rechten Rand
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat4)
                        , style "width" "100%"
                        , style "height" "100%"
                        , style "zIndex" "1"
                        , style "background" "none"
                        , style "border" "none"
                        , style "padding" "0"
                        , style "cursor" "pointer"
                        , style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat4.name)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [ style "position" "absolute"
                    , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , style "bottom" "50%"
                    , style "right" "21.5%"  -- 0.5% vom rechten Rand
                    , style "color" "white"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"
                    , style "padding" "10px"
                    , style "border-radius" "5px"
                    , style "zIndex" "1"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    , style "justify-content" "space-between"
                    ]
                    [ div [] [ text model.seat4.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat4)
                        , style "width" "50%"
                        , style "height" "10%"
                        , style "zIndex" "1"
                        , style "cursor" "pointer"
                        , style "top" "90%"
                        , style "left" "0%"
                        ] 
                        [ text "Gehen sie bitte!" ]
                    ]
                ]
                  else
                    text ""
        , button --Seat5
            [ Html.Events.onClick (NPCClicked model.seat5)
            , if (model.seat5.hidden) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "right" "1.4%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (model.seat5.name)
                , if (model.seat5.hidden) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , if model.seat5.modal then
            div [ style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "width" "100%"
                , style "height" "100%"
                , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "zIndex" "2"
            ]
                [ div
                    [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , style "position" "absolute"
                    , style "bottom" "13.8%"  -- 13.8% vom unteren Rand
                    , style "right" "1.4%"  -- 1.4% vom rechten Rand
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat5)
                        , style "width" "100%"
                        , style "height" "100%"
                        , style "zIndex" "1"
                        , style "background" "none"
                        , style "border" "none"
                        , style "padding" "0"
                        , style "cursor" "pointer"
                        , style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat5.name)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [ style "position" "absolute"
                    , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , style "bottom" "50%"
                    , style "right" "1.4%"
                    , style "color" "white"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"
                    , style "padding" "10px"
                    , style "border-radius" "5px"
                    , style "zIndex" "1"
                    , style "display" "flex"
                    , style "flex-direction" "column"
                    , style "justify-content" "space-between"
                    ]
                    [ div [] [ text model.seat5.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat5)
                        , style "width" "50%"
                        , style "height" "10%"
                        , style "zIndex" "1"
                        , style "cursor" "pointer"
                        , style "top" "90%"
                        , style "left" "0%"
                        ] 
                        [ text "Gehen sie bitte!" ]
                    ]
                ]
            else
                text ""
        , if model.timeChoosen == False then
            div 
            [ class "overlay"]
            [input  [ placeholder "Bitte geben sie an wie lange sie lernen wollen."
                    , value model.userInput
                    , onInput GetInput 
                    , class "input-field"
                    ] []
            , div [class "display-text"] 
                [ text ("Ich möchte für " ++ model.userInput ++" Minuten lernen") ]
            , button    [ class "confirm-button"
                        , Html.Events.onClick SwitchOverlay 
                        , if checkForInt model.userInput then style "background-color" "rgb(0, 255, 0)" else style "background-color" "rgb(255, 0, 0)"
                        , disabled (not (checkForInt model.userInput))
                        ] 
                        [ if checkForInt model.userInput then text "Confirm" else text "Bitte richtig eingeben!" ]
            ]
        else
            div 
            [ class "overlay"]
            [ div [class "display-text"] 
                [ text ("Sie haben noch " ++ String.fromInt model.time ++" Minuten bis zum ersten Besuch") ]
            , button    [ class "confirm-button"
                        , Html.Events.onClick SwitchOverlay 
                        ] 
                        [ text "Ich brauche eine Pause." ]
            ]  
        ]


-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }