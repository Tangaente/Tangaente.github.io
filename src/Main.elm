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
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array exposing (..)
import Http
import Json.Decode as Decode
import Maybe exposing (withDefault)


-- Typ-Deklarationen

type alias Seat = 
    { name : String 
    , randomString : Maybe String
    , hidden : Bool
    , modal : Bool
    , id : Int
    , nextText : String --Für Dialoge
    , spokenText : String
    , index : Int -- Für Dialoge abspielen
    , conversation : Int --Punkt der Konversation
    , active : Bool
    , waiting : Int
    }

type alias RandomValues =
    { randomIndex : Int
    , randomSeat : Int
    }

type alias Dialogue =
    Array (Array String)

type alias Dialogues =
    { person1 : Dialogue
    , person2 : Dialogue
    , person3 : Dialogue
    , person4 : Dialogue
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
    , showModal : Bool
    , userInput : String
    , userInput2 : String
    , timeChoosen : Bool
    , time : Int
    , daten : Array Float
    , arbeiten : Array String
    , dialogues : Maybe Dialogues
    , visitTimes : List Int --Liste mit Daten darüber wie oft die jeweiligen Personen schon da waren 
    }


--Initialisierung

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 
    10 
    10 
    True 
    {name = "/public/images/Random_Person.png", randomString = Nothing, hidden = True, modal = False, id = 0, nextText = "Next Text", spokenText = "", index = 0, conversation = 0,active = True, waiting = 0} 
    {name = "/public/images/Random_Person.png", randomString = Nothing, hidden = True, modal = False, id = 1, nextText = "Next Text", spokenText = "", index = 0, conversation = 0,active = True, waiting = 0} 
    {name = "/public/images/Random_Person.png", randomString = Nothing, hidden = True, modal = False, id = 2, nextText = "Next Text", spokenText = "", index = 0, conversation = 0,active = True, waiting = 0} 
    {name = "/public/images/Random_Person.png", randomString = Nothing, hidden = True, modal = False, id = 3, nextText = "Next Text", spokenText = "", index = 0, conversation = 0,active = True, waiting = 0} 
    {name = "/public/images/Random_Person.png", randomString = Nothing, hidden = True, modal = False, id = 4, nextText = "Next Text", spokenText = "", index = 0, conversation = 0,active = True, waiting = 0} 
    0 
    ["/public/images/Person1.png","/public/images/Person2.png","/public/images/Person3.png","/public/images/Person4.png"] 
    ["0","1","2","3","4"]  
    False
    "..."
    "..."
    False
    0
    (Array.fromList [0])
    (Array.fromList ["Pause"])
    Nothing
    [0,0,0,0]
    , fetchDialogues )

-- Message Typen

type Msg
    = WindowResized (List Int)
    | PrepNextNPC
    | NPCClicked Seat
    | GotRandomValues RandomValues
    | Tick Posix --Dialogfenster, dadurch wird Dialog nach und nach angezeigt
    | RemoveNPC Seat
    | GetInput String
    | GetInput2 String
    | TickMinute Posix
    | TickWaiting Posix
    | SwitchOverlay
    | FetchDialoguesSuccess Dialogues
    | FetchDialoguesFailure
    | NextDialogue Seat

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

drawBars : Array Float -> List String -> Model -> List (Svg msg)
drawBars dataPoints names model =
    let
        barWidth = 40
        spaceBetweenBars = 20
        initialX = 30
        xlength = (Array.length model.daten * 60 + 70)
        maxData = List.maximum (Array.toList dataPoints) |> Maybe.withDefault 0
        initialY = maxData + 40
        bars = List.indexedMap
            (\index height ->
                Svg.rect
                    [ x (String.fromFloat (initialX + (barWidth + spaceBetweenBars) * (index |> toFloat)))
                    , y (String.fromFloat (initialY - height))
                    , Svg.Attributes.width (String.fromFloat barWidth)
                    , Svg.Attributes.height (String.fromFloat (height))
                    , fill "white"
                    ]
                    []
            )
            (Array.toList(dataPoints))
        barLabels = List.indexedMap
            (\index name ->
                Svg.text_
                    [ x (String.fromFloat (initialX + (barWidth + spaceBetweenBars) * (index |> toFloat) + (barWidth / 2)))
                    , y (String.fromFloat (initialY + 20))
                    , fill "white"
                    , fontSize "10"
                    , textAnchor "middle"
                    ]
                    [ Html.text name ]
            )
            names
        xAxis = Svg.line
            [ x1 "0", y1 (String.fromFloat initialY), x2 (String.fromInt xlength), y2 (String.fromFloat initialY), stroke "white", strokeWidth "2" ]
            []
        yAxis = Svg.line
            [ x1 "0", y1 "0", x2 "0", y2 (String.fromFloat initialY), stroke "white", strokeWidth "2" ]
            []
        xAxisLabel = Svg.text_
            [ x (String.fromInt xlength), y (String.fromFloat initialY), fill "white", fontSize "10" ]
            [ Html.text "Arbeit" ]
        yAxisLabel = Svg.text_
            [ x "10", y "10", fill "white", fontSize "10" ]
            [ Html.text "Minuten" ]
        originLabel = Svg.text_
            [ x "5", y (String.fromFloat (initialY + 10)), fill "white", fontSize "10" ]
            [ Html.text "0" ]
    in
    [ xAxis, yAxis, xAxisLabel, yAxisLabel, originLabel ] ++ bars ++ barLabels

    -- Findet Index eines Elementes eines Arrays 
findIndex : String -> Array String -> Maybe Int
findIndex target arr =
    Array.toList arr
        |> List.indexedMap (\i x -> if x == target then Just i else Nothing)
        |> List.filterMap identity
        |> List.head

-- Fetch the dialogues data for both persons
fetchDialogues : Cmd Msg
fetchDialogues =
    Http.get
        { url = "/public/data/dialogues.json"
        , expect = Http.expectJson handleFetchResult dialoguesDecoder
        }

-- Define a JSON decoder for your dialogues
dialoguesDecoder : Decode.Decoder Dialogues
dialoguesDecoder =
    Decode.map4 Dialogues
        (Decode.field "person1" (Decode.array (Decode.array Decode.string)))
        (Decode.field "person2" (Decode.array (Decode.array Decode.string)))
        (Decode.field "person3" (Decode.array (Decode.array Decode.string)))
        (Decode.field "person4" (Decode.array (Decode.array Decode.string)))


-- Handle the result of the HTTP request
handleFetchResult : Result Http.Error Dialogues -> Msg
handleFetchResult result =
    case result of
        Ok dialogues ->
            FetchDialoguesSuccess dialogues

        Err _ ->
            FetchDialoguesFailure

getNextText : Model -> Seat -> String
getNextText model seat = 
    case model.dialogues of 
        Just dialoge ->
            case seat.name of 
                "/public/images/Person1.png" ->
                    let 
                        newArray = dialoge.person1
                        visitAmount = withDefault 0 (get 0 (Array.fromList model.visitTimes))
                    in 
                    case get seat.conversation newArray of 
                        Just a ->
                            case get visitAmount a of 
                                Just b ->
                                    b
                                Nothing ->
                                    ""
                        Nothing ->
                            ""
                "/public/images/Person2.png" ->
                    let 
                        newArray = dialoge.person2
                        visitAmount = withDefault 0 (get 1 (Array.fromList model.visitTimes))
                    in 
                    case get seat.conversation newArray of 
                        Just a ->
                            case get visitAmount a of 
                                Just b ->
                                    b
                                Nothing ->
                                    ""
                        Nothing ->
                            ""
                "/public/images/Person3.png" ->
                    let 
                        newArray = dialoge.person3
                        visitAmount = withDefault 0 (get 2 (Array.fromList model.visitTimes))
                    in 
                    case get seat.conversation newArray of 
                        Just a ->
                            case get visitAmount a of 
                                Just b ->
                                    b
                                Nothing ->
                                    ""
                        Nothing ->
                            ""
                "/public/images/Person4.png" ->
                    let 
                        newArray = dialoge.person4
                        visitAmount = withDefault 0 (get 3 (Array.fromList model.visitTimes))
                    in 
                    case get seat.conversation newArray of 
                        Just a ->
                            case get visitAmount a of 
                                Just b ->
                                    b
                                Nothing ->
                                    ""
                        Nothing ->
                            ""
                _ ->
                   ""
        Nothing ->
            ""

getVistorList: Model -> Seat -> List Int --Make new List for mode.visitTimes
getVistorList model seat = 


    if seat.conversation == 3 then 
                case seat.name of 
                    "/public/images/Person1.png" -> 
                        [withDefault 0 (get 0 (Array.fromList model.visitTimes)) + 1, withDefault 0 (get 1 (Array.fromList model.visitTimes)), withDefault 0 (get 2 (Array.fromList model.visitTimes)), withDefault 0 (get 3 (Array.fromList model.visitTimes))]
                    "/public/images/Person2.png" ->
                        [withDefault 0 (get 0 (Array.fromList model.visitTimes)), withDefault 0 (get 1 (Array.fromList model.visitTimes)) + 1, withDefault 0 (get 2 (Array.fromList model.visitTimes)), withDefault 0 (get 3 (Array.fromList model.visitTimes))]
                    "/public/images/Person3.png" -> 
                        [withDefault 0 (get 0 (Array.fromList model.visitTimes)), withDefault 0(get 1 (Array.fromList model.visitTimes)), withDefault 0(get 2 (Array.fromList model.visitTimes)) + 1, withDefault 0(get 3 (Array.fromList model.visitTimes))]
                    "/public/images/Person4.png" ->
                        [withDefault 0 (get 0 (Array.fromList model.visitTimes)), withDefault 0(get 1 (Array.fromList model.visitTimes)), withDefault 0(get 2 (Array.fromList model.visitTimes)), withDefault 0(get 3 (Array.fromList model.visitTimes)) + 1]
                    _ -> 
                        [withDefault 0 (get 0 (Array.fromList model.visitTimes)) + 1, withDefault 0 (get 1 (Array.fromList model.visitTimes)), withDefault 0 (get 2 (Array.fromList model.visitTimes)), withDefault 0 (get 3 (Array.fromList model.visitTimes))]
            else 
                [withDefault 0 (get 0 (Array.fromList model.visitTimes)), withDefault 0 (get 1 (Array.fromList model.visitTimes)), withDefault 0 (get 2 (Array.fromList model.visitTimes)), withDefault 0 (get 3 (Array.fromList model.visitTimes))]
    


    
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
            let 
                makeSeat : Seat -> String -> Seat
                makeSeat b a =
                    {b | name = a, hidden = False, modal = False}

                talkPerson : Seat -> Seat
                talkPerson c =
                    {c | modal = not c.modal, spokenText = "", index = 0, nextText = getNextText model seat}
            in
                if seat.name == "/public/images/Random_Person.png" then --Person wird zum enthülle angeklickt
                    case seat.id of 
                        0 ->
                            case seat.randomString of 
                                Just a -> 
                                    ( { model | seat1 = makeSeat model.seat1 a }, Cmd.none )
                                Nothing ->
                                    ( { model | seat1 = makeSeat model.seat1 "/public/images/Random_Person.png" }, Cmd.none )
                        1 ->
                            case seat.randomString of 
                                Just a -> 
                                    ( { model | seat2 = makeSeat model.seat2 a }, Cmd.none )
                                Nothing ->
                                    ( { model | seat2 = makeSeat model.seat2 "/public/images/Random_Person.png" }, Cmd.none )
                        2 ->
                            case seat.randomString of 
                                Just a -> 
                                    ( { model | seat3 = makeSeat model.seat3 a }, Cmd.none )
                                Nothing ->
                                    ( { model | seat3 = makeSeat model.seat3 "/public/images/Random_Person.png" }, Cmd.none )
                        3 ->
                            case seat.randomString of 
                                Just a -> 
                                    ( { model | seat4 = makeSeat model.seat4 a }, Cmd.none )
                                Nothing ->
                                    ( { model | seat4 = makeSeat model.seat4 "/public/images/Random_Person.png" }, Cmd.none )
                        4 ->
                            case seat.randomString of 
                                Just a -> 
                                    ( { model | seat5 = makeSeat model.seat5 a }, Cmd.none )
                                Nothing ->
                                    ( { model | seat5 = makeSeat model.seat5 "/public/images/Random_Person.png" }, Cmd.none )
                        _ -> 
                            case seat.randomString of 
                                Just a -> 
                                    ( { model | seat1 = makeSeat model.seat1 a }, Cmd.none )
                                Nothing ->
                                    ( { model | seat1 = makeSeat model.seat1 "/public/images/Random_Person.png" }, Cmd.none )

                    else --Person wird zum sprechen angeklickt

                         case seat.id of 
                        0 ->
                                ( { model | seat1 = talkPerson model.seat1}, Cmd.none )
                        1 ->
                                ( { model | seat2 = talkPerson model.seat2}, Cmd.none )
                        2 ->
                                ( { model | seat3 = talkPerson model.seat3}, Cmd.none )
                                
                        3 ->
                                ( { model | seat4 = talkPerson model.seat4}, Cmd.none )
                                
                        4 ->
                                ( { model | seat5 = talkPerson model.seat5}, Cmd.none )
                                
                        _ -> 
                                ( { model | seat1 = talkPerson model.seat1}, Cmd.none )

        GotRandomValues randomValues ->
            let
                randomStr = List.Extra.getAt randomValues.randomIndex model.person_list
                randomSeat = List.Extra.getAt randomValues.randomSeat model.seat_list

                seatNHM : Seat -> Seat
                seatNHM a =
                    {a | name = "/public/images/Random_Person.png", hidden = False, modal = False, randomString = randomStr, active = True}
            in
            case randomStr of 
                Just b ->
                    case randomSeat of 
                        Just a -> 
                            if a == "0" then
                                    ( { model | seat1 = seatNHM model.seat1, nextSeat = 0, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..."}, Cmd.none )
                            else if a == "1" then
                                    ( { model | seat2 = seatNHM model.seat2, nextSeat = 1, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                            else if a == "2" then
                                    ( { model | seat3 = seatNHM model.seat3, nextSeat = 2, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                            else if a == "3" then
                                    ( { model | seat4 = seatNHM model.seat4, nextSeat = 3, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                            else if a == "4" then
                                    ( { model | seat5 = seatNHM model.seat5, nextSeat = 4, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                            else
                                    ( { model | seat1 = seatNHM model.seat1, nextSeat = 0, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list, timeChoosen = not model.timeChoosen, userInput = "..." }, Cmd.none )
                        Nothing ->
                            ( { model | nextSeat = 1, person_list = removeWord randomStr model.person_list, timeChoosen = not model.timeChoosen, userInput = "..."}, Cmd.none )
                Nothing ->
                    ( { model | nextSeat = 1, timeChoosen = not model.timeChoosen, userInput = "..."}, Cmd.none )
                    
        Tick newTime ->
            if model.seat1.modal == True then
                let
                    newIndex = model.seat1.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat1.nextText

                    seatSI: Seat -> Seat 
                    seatSI a = 
                        {a | spokenText = newDisplayedText, index = newIndex}
                in
                if newIndex <= String.length model.seat1.nextText then
                    ( { model | seat1 = seatSI model.seat1}, Cmd.none)
                else
                    ( model, Cmd.none )
            else if model.seat2.modal == True then
                let
                    newIndex = model.seat2.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat2.nextText

                    seatSI: Seat -> Seat 
                    seatSI a = 
                        {a | spokenText = newDisplayedText, index = newIndex}
                in
                if newIndex <= String.length model.seat2.nextText then
                    ( { model | seat2 = seatSI model.seat2}, Cmd.none)
                else
                    ( model, Cmd.none )
            else if model.seat3.modal == True then
                let
                    newIndex = model.seat3.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat3.nextText

                    seatSI: Seat -> Seat 
                    seatSI a = 
                        {a | spokenText = newDisplayedText, index = newIndex}
                in
                if newIndex <= String.length model.seat3.nextText then
                    ( { model | seat3 = seatSI model.seat3}, Cmd.none)
                else
                    ( model, Cmd.none )
            else if model.seat4.modal == True then
                let
                    newIndex = model.seat4.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat4.nextText

                    seatSI: Seat -> Seat 
                    seatSI a = 
                        {a | spokenText = newDisplayedText, index = newIndex}
                in
                if newIndex <= String.length model.seat4.nextText then
                    ( { model | seat4 = seatSI model.seat4}, Cmd.none)
                else
                    ( model, Cmd.none )
            else if model.seat5.modal == True then
                let
                    newIndex = model.seat5.index + 1
                    newDisplayedText = String.slice 0 newIndex model.seat5.nextText

                    seatSI: Seat -> Seat 
                    seatSI a = 
                        {a | spokenText = newDisplayedText, index = newIndex}
                in
                if newIndex <= String.length model.seat5.nextText then
                    ( { model | seat5 = seatSI model.seat5}, Cmd.none)
                else
                    ( model, Cmd.none )
            else 
                ( model, Cmd.none )

        RemoveNPC seat -> 
            let 
                seatClear: Seat -> Seat 
                seatClear a =
                    {a | name = "/public/images/Random_Person.png", hidden = True, modal = False, nextText = "Next Text", spokenText = "", index = 0, conversation = 0, active = True}
            in
            case seat.id of 
                        0 ->
                                ( { model | person_list = List.append model.person_list ([seat.name]),seat_list = List.append model.seat_list ([String.fromInt seat.id]),seat1 = seatClear model.seat1, visitTimes = getVistorList model model.seat1}, Cmd.none )
                        1 ->
                                ( { model | person_list = List.append model.person_list ([seat.name]),seat_list = List.append model.seat_list ([String.fromInt seat.id]),seat2 = seatClear model.seat2, visitTimes = getVistorList model model.seat2}, Cmd.none )
                        2 ->
                                ( { model | person_list = List.append model.person_list ([seat.name]),seat_list = List.append model.seat_list ([String.fromInt seat.id]),seat3 = seatClear model.seat3, visitTimes = getVistorList model model.seat3}, Cmd.none )
                        3 ->
                                ( { model | person_list = List.append model.person_list ([seat.name]),seat_list = List.append model.seat_list ([String.fromInt seat.id]),seat4 = seatClear model.seat4, visitTimes = getVistorList model model.seat4}, Cmd.none )
                        4 ->
                                ( { model | person_list = List.append model.person_list ([seat.name]),seat_list = List.append model.seat_list ([String.fromInt seat.id]),seat5 = seatClear model.seat5, visitTimes = getVistorList model model.seat5}, Cmd.none )
                        _ -> 
                                ( { model | person_list = List.append model.person_list ([seat.name]),seat_list = List.append model.seat_list ([String.fromInt seat.id]),seat1 = seatClear model.seat1, visitTimes = getVistorList model model.seat1}, Cmd.none )
        
        GetInput input ->
            ({model | userInput = input}, Cmd.none )

        GetInput2 input ->
            ({model | userInput2 = input}, Cmd.none )

        TickMinute _ ->
            let
                    newTime = model.time - 1
                    oldTime = get 0 model.daten
                    raw = String.toInt model.userInput
                    targetTime =  Maybe.withDefault 0 raw
                    timeWorked = targetTime - model.time
                    index = findIndex model.userInput2 model.arbeiten           
            in
            if model.timeChoosen && newTime /= 0 then
                ({ model | time = model.time - 1 }, Cmd.none )
            else if model.timeChoosen && newTime == 0 then
                case index of 
                    Just b-> 
                        case get b model.daten of 
                            Just c->
                                update PrepNextNPC {model | daten = Array.set b (c + toFloat timeWorked) model.daten }
                            Nothing ->
                                (model, Cmd.none)
                    Nothing ->
                        update PrepNextNPC {model | arbeiten = Array.append model.arbeiten (Array.fromList [model.userInput2]), daten = Array.append model.daten (Array.fromList[toFloat timeWorked]) }
            else 
                case oldTime of 
                    Just a ->
                        ({ model | daten = Array.set 0 (a + 1) model.daten }, Cmd.none )
                    Nothing ->
                        (model, Cmd.none)

        TickWaiting _ -> 
            let 
                newTime1 = Basics.max -30 (model.seat1.waiting - 1)
                newTime2 = Basics.max -30 (model.seat2.waiting - 1)
                newTime3 = Basics.max -30 (model.seat3.waiting - 1)
                newTime4 = Basics.max -30 (model.seat4.waiting - 1)
                newTime5 = Basics.max -30 (model.seat5.waiting - 1)

                updateTime: Seat -> Int -> Seat 
                updateTime seat time = 
                    {seat | waiting = time}
            in 
            if newTime1 == 0 && model.seat1.active == False then 
                update (RemoveNPC model.seat1) {model | seat1 = updateTime model.seat1 newTime1, seat2 = updateTime model.seat2 newTime2, seat3 = updateTime model.seat3 newTime3, seat4 = updateTime model.seat4 newTime4, seat5 = updateTime model.seat5 newTime5}
            else if newTime2 == 0 && model.seat2.active == False then 
                update (RemoveNPC model.seat2) {model | seat1 = updateTime model.seat1 newTime1, seat2 = updateTime model.seat2 newTime2, seat3 = updateTime model.seat3 newTime3, seat4 = updateTime model.seat4 newTime4, seat5 = updateTime model.seat5 newTime5}
            else if newTime3 == 0 && model.seat3.active == False then 
                update (RemoveNPC model.seat3) {model | seat1 = updateTime model.seat1 newTime1, seat2 = updateTime model.seat2 newTime2, seat3 = updateTime model.seat3 newTime3, seat4 = updateTime model.seat4 newTime4, seat5 = updateTime model.seat5 newTime5}
            else if newTime4 == 0 && model.seat4.active == False then 
                update (RemoveNPC model.seat4) {model | seat1 = updateTime model.seat1 newTime1, seat2 = updateTime model.seat2 newTime2, seat3 = updateTime model.seat3 newTime3, seat4 = updateTime model.seat4 newTime4, seat5 = updateTime model.seat5 newTime5}
            else if newTime5 == 0 && model.seat5.active == False then 
                update (RemoveNPC model.seat5) {model | seat1 = updateTime model.seat1 newTime1, seat2 = updateTime model.seat2 newTime2, seat3 = updateTime model.seat3 newTime3, seat4 = updateTime model.seat4 newTime4, seat5 = updateTime model.seat5 newTime5}
            else 
                ({model | seat1 = updateTime model.seat1 newTime1, seat2 = updateTime model.seat2 newTime2, seat3 = updateTime model.seat3 newTime3, seat4 = updateTime model.seat4 newTime4, seat5 = updateTime model.seat5 newTime5}, Cmd.none)
                

        SwitchOverlay ->
            if model.timeChoosen then
                 let 
                    raw = String.toInt model.userInput
                    targetTime =  Maybe.withDefault 0 raw
                    timeWorked = targetTime - model.time
                    index = findIndex model.userInput2 model.arbeiten
                in
                case index of 
                    Just b-> 
                        case get b model.daten of 
                            Just c->
                                update PrepNextNPC {model | daten = Array.set b (c + toFloat timeWorked) model.daten }
                            Nothing ->
                                (model, Cmd.none)
                    Nothing ->
                        update PrepNextNPC {model | arbeiten = Array.append model.arbeiten (Array.fromList [model.userInput2]), daten = Array.append model.daten (Array.fromList[toFloat timeWorked]) }
            else
                case String.toInt model.userInput of 
                    Just a ->
                        ({ model | timeChoosen = not model.timeChoosen, time = a}, Cmd.none )
                    Nothing ->
                        (model, Cmd.none )

        FetchDialoguesSuccess dialogues ->
            ( { model | dialogues = Just dialogues }, Cmd.none )

        FetchDialoguesFailure ->
            ( model, Cmd.none )

        NextDialogue seat ->
            let 
                seatC: Seat -> Seat 
                seatC b = 
                    {b | conversation = b.conversation + 1}

                nextDialogue: Seat -> Seat 
                nextDialogue a =
                    {a | nextText = getNextText model (seatC seat), spokenText = "", index = 0}

                seatAMCT: Seat -> Seat 
                seatAMCT c = 
                    {c | active = False, modal = False, conversation = c.conversation + 1, waiting = 30}

            in
            if seat.conversation < 2 then
                case seat.id of 
                    0 -> 
                        ({model| seat1 = (nextDialogue (seatC seat))}, Cmd.none)
                    1 -> 
                        ({model| seat2 = (nextDialogue (seatC seat))}, Cmd.none)
                    2 -> 
                        ({model| seat3 = (nextDialogue (seatC seat))}, Cmd.none)
                    3 -> 
                        ({model| seat4 = (nextDialogue (seatC seat))}, Cmd.none)
                    4 -> 
                        ({model| seat5 = (nextDialogue (seatC seat))}, Cmd.none)
                    _ -> 
                        ({model| seat1 = (nextDialogue (seatC seat))}, Cmd.none)
            else 
                 case seat.id of 
                    0 -> 
                        ({model| seat1 = (seatAMCT seat)}, Cmd.none)
                    1 -> 
                        ({model| seat2 = (seatAMCT seat)}, Cmd.none)
                    2 -> 
                        ({model| seat3 = (seatAMCT seat)}, Cmd.none)
                    3 -> 
                        ({model| seat4 = (seatAMCT seat)}, Cmd.none)
                    4 -> 
                        ({model| seat5 = (seatAMCT seat)}, Cmd.none)
                    _ -> 
                        ({model| seat1 = (seatAMCT seat)}, Cmd.none)          
-- SUBSCRIPTIONS

port windowSize : (List Int -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [Time.every 50 Tick --Gespräche
    ,Time.every (60 * 1000) TickMinute --Minuten Timer
    ,Time.every (60 * 1000) TickWaiting --Waiting Timer Update
    ,windowSize WindowResized --Bildschirmgröße
    ]

-- VIEW

view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        ]
        [ img
            [ src "/public/images/Theke.gif"
            , Html.Attributes.style "height" <| String.fromInt model.height ++ "px" 
            , Html.Attributes.style "width" <| String.fromInt model.width ++ "px" 
            ]
            []
        , button --Seat1
            [ Html.Events.onClick (NPCClicked model.seat1)
            , if model.seat1.hidden == True then hidden True else hidden False
            , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , Html.Attributes.style "left" "1.4%"  -- 0.5% vom rechten Rand
            , Html.Attributes.style "background" "none"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "zIndex" "1"
            , Html.Attributes.disabled (not model.seat1.active)
            ] 
            [ img 
                [ src (model.seat1.name)
                , if model.seat1.hidden == True then hidden True else hidden False
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                ] 
                []
            ]
        , if model.seat1.modal then --Seat1 Talking Scene
                div [ Html.Attributes.style "position" "fixed"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "justify-content" "center"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "zIndex" "2"
            ]
                [ div
                    [ Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                    , Html.Attributes.style "left" "1.4%"  -- 0.5% vom rechten Rand
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat1)
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "background" "none"
                        , Html.Attributes.style "border" "none"
                        , Html.Attributes.style "padding" "0"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat1.name)
                            , Html.Attributes.style "width" "100%"
                            , Html.Attributes.style "height" "100%"
                            , Html.Attributes.style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , Html.Attributes.style "bottom" "50%"
                    , Html.Attributes.style "left" "1.4%"  -- 0.5% vom rechten Rand
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"
                    , Html.Attributes.style "padding" "10px"
                    , Html.Attributes.style "border-radius" "5px"
                    , Html.Attributes.style "zIndex" "1"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "space-between"
                    ]
                    [ div [] [ Html.text model.seat1.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat1)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "left" "0%"
                        ] 
                        [ Html.text "Gehen sie bitte!" ]
                    ,button 
                        [ Html.Events.onClick (NextDialogue model.seat1)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "right" "0%"
                        ] 
                        [   if model.seat1.conversation == 0 then
                                Html.text "Hello."
                            else if model.seat1.conversation == 1 then
                                Html.text "Bestellung kommt sofort"
                            else if model.seat1.conversation == 2 then
                                Html.text "Lassen sie es sich schmecken"
                            else 
                                Html.text "??"
                        ]
                    ]
                ]
                  else
                    Html.text ""
        , button --Seat2
            [ Html.Events.onClick (NPCClicked model.seat2)
            , if (model.seat2.hidden) == True then hidden True else hidden False
            , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , Html.Attributes.style "left" "21.5%"  -- 0.5% vom rechten Rand
            , Html.Attributes.style "background" "none"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "zIndex" "1"
            , Html.Attributes.disabled (not model.seat2.active)
            ] 
            [ img 
                [ src (model.seat2.name)
                , if (model.seat2.hidden) == True then hidden True else hidden False
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                ] 
                []
            ]
       , if model.seat2.modal then --Seat2 Talking Scene
                div [ Html.Attributes.style "position" "fixed"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "justify-content" "center"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "zIndex" "2"
            ]
                [ div
                    [ Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                    , Html.Attributes.style "left" "21.5%"  -- 0.5% vom rechten Rand
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat2)
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "background" "none"
                        , Html.Attributes.style "border" "none"
                        , Html.Attributes.style "padding" "0"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat2.name)
                            , Html.Attributes.style "width" "100%"
                            , Html.Attributes.style "height" "100%"
                            , Html.Attributes.style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , Html.Attributes.style "bottom" "50%"
                    , Html.Attributes.style "left" "21.5%"  -- 0.5% vom rechten Rand
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"
                    , Html.Attributes.style "padding" "10px"
                    , Html.Attributes.style "border-radius" "5px"
                    , Html.Attributes.style "zIndex" "1"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "space-between"
                    ]
                    [ div [] [ Html.text model.seat2.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat2)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "left" "0%"
                        ] 
                        [ Html.text "Gehen sie bitte!" ]
                    ,button 
                        [ Html.Events.onClick (NextDialogue model.seat2)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "right" "0%"
                        ] 
                        [   if model.seat2.conversation == 0 then
                                Html.text "Hello."
                            else if model.seat2.conversation == 1 then
                                Html.text "Bestellung kommt sofort"
                            else if model.seat2.conversation == 2 then
                                Html.text "Lassen sie es sich schmecken"
                            else 
                                Html.text "??"
                        ]
                    ]
                ]
                  else
                    Html.text ""
        , button --Seat3
            [ Html.Events.onClick (NPCClicked model.seat3)
            , if (model.seat3.hidden) == True then hidden True else hidden False
            , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , Html.Attributes.style "right" "41.5%"  -- 0.5% vom rechten Rand
            , Html.Attributes.style "background" "none"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "zIndex" "1"
            , Html.Attributes.disabled (not model.seat3.active)
            ] 
            [ img 
                [ src (model.seat3.name)
                , if (model.seat3.hidden) == True then hidden True else hidden False
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                ] 
                []
            ]
        , if model.seat3.modal then --Seat3 Talking Scene
                div [ Html.Attributes.style "position" "fixed"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "justify-content" "center"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "zIndex" "2"
            ]
                [ div
                    [ Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                    , Html.Attributes.style "right" "41.5%"  -- 0.5% vom rechten Rand
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat3)
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "background" "none"
                        , Html.Attributes.style "border" "none"
                        , Html.Attributes.style "padding" "0"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat3.name)
                            , Html.Attributes.style "width" "100%"
                            , Html.Attributes.style "height" "100%"
                            , Html.Attributes.style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , Html.Attributes.style "bottom" "50%"
                    , Html.Attributes.style "right" "41.5%"  -- 0.5% vom rechten Rand
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"
                    , Html.Attributes.style "padding" "10px"
                    , Html.Attributes.style "border-radius" "5px"
                    , Html.Attributes.style "zIndex" "1"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "space-between"
                    ]
                    [ div [] [ Html.text model.seat3.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat3)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "left" "0%"
                        ] 
                        [ Html.text "Gehen sie bitte!" ]
                    ,button 
                        [ Html.Events.onClick (NextDialogue model.seat3)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "right" "0%"
                        ] 
                        [   if model.seat3.conversation == 0 then
                                Html.text "Hello."
                            else if model.seat3.conversation == 1 then
                                Html.text "Bestellung kommt sofort"
                            else if model.seat3.conversation == 2 then
                                Html.text "Lassen sie es sich schmecken"
                            else 
                                Html.text "??"
                        ]
                    ]
                ]
                  else
                    Html.text ""
        , button --Seat4
            [ Html.Events.onClick (NPCClicked model.seat4)
            , if (model.seat4.hidden) == True then hidden True else hidden False
            , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , Html.Attributes.style "right" "21.5%"  -- 0.5% vom rechten Rand
            , Html.Attributes.style "background" "none"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "zIndex" "1"
            , Html.Attributes.disabled (not model.seat4.active)
            ] 
            [ img 
                [ src (model.seat4.name)
                , if (model.seat4.hidden) == True then hidden True else hidden False
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                ] 
                []
            ]
         , if model.seat4.modal then --Seat4 Talking Scene
                div [ Html.Attributes.style "position" "fixed"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "justify-content" "center"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "zIndex" "2"
            ]
                [ div
                    [ Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                    , Html.Attributes.style "right" "21.5%"  -- 0.5% vom rechten Rand
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat4)
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "background" "none"
                        , Html.Attributes.style "border" "none"
                        , Html.Attributes.style "padding" "0"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat4.name)
                            , Html.Attributes.style "width" "100%"
                            , Html.Attributes.style "height" "100%"
                            , Html.Attributes.style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , Html.Attributes.style "bottom" "50%"
                    , Html.Attributes.style "right" "21.5%"  -- 0.5% vom rechten Rand
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"
                    , Html.Attributes.style "padding" "10px"
                    , Html.Attributes.style "border-radius" "5px"
                    , Html.Attributes.style "zIndex" "1"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "space-between"
                    ]
                    [ div [] [ Html.text model.seat4.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat4)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "left" "0%"
                        ] 
                        [ Html.text "Gehen sie bitte!" ]
                    ,button 
                        [ Html.Events.onClick (NextDialogue model.seat4)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "right" "0%"
                        ] 
                        [   if model.seat4.conversation == 0 then
                                Html.text "Hello."
                            else if model.seat4.conversation == 1 then
                                Html.text "Bestellung kommt sofort"
                            else if model.seat4.conversation == 2 then
                                Html.text "Lassen sie es sich schmecken"
                            else 
                                Html.text "??"
                        ]
                    ]
                ]
                  else
                    Html.text ""
        , button --Seat5
            [ Html.Events.onClick (NPCClicked model.seat5)
            , if (model.seat5.hidden) == True then hidden True else hidden False
            , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , Html.Attributes.style "right" "1.4%"  -- 0.5% vom rechten Rand
            , Html.Attributes.style "background" "none"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "zIndex" "1"
            , Html.Attributes.disabled (not model.seat5.active)
            ] 
            [ img 
                [ src (model.seat5.name)
                , if (model.seat5.hidden) == True then hidden True else hidden False
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                ] 
                []
            ]
        , if model.seat5.modal then
            div [ Html.Attributes.style "position" "fixed"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "justify-content" "center"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "zIndex" "2"
            ]
                [ div
                    [ Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "bottom" "13.8%"  -- 13.8% vom unteren Rand
                    , Html.Attributes.style "right" "1.4%"  -- 1.4% vom rechten Rand
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "zIndex" "3"
                    ]
                    [ button
                        [ Html.Events.onClick (NPCClicked model.seat5)
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "background" "none"
                        , Html.Attributes.style "border" "none"
                        , Html.Attributes.style "padding" "0"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "position" "relative"
                        ] 
                        [ img 
                            [ src (model.seat5.name)
                            , Html.Attributes.style "width" "100%"
                            , Html.Attributes.style "height" "100%"
                            , Html.Attributes.style "zIndex" "1"
                            ] 
                            []
                        ]

                    ]
                    , div 
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                    , Html.Attributes.style "height" <| String.fromFloat (toFloat model.height * 0.3 + 100.0) ++ "px"
                    , Html.Attributes.style "bottom" "50%"
                    , Html.Attributes.style "right" "1.4%"
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "background-color" "rgba(0, 0, 0, 0.5)"
                    , Html.Attributes.style "padding" "10px"
                    , Html.Attributes.style "border-radius" "5px"
                    , Html.Attributes.style "zIndex" "1"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "space-between"
                    ]
                    [ div [] [ Html.text model.seat5.spokenText ]
                    , button 
                        [ Html.Events.onClick (RemoveNPC model.seat5)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "left" "0%"
                        ] 
                        [ Html.text "Gehen sie bitte!" ]
                    ,button 
                        [ Html.Events.onClick (NextDialogue model.seat5)
                        , Html.Attributes.style "position" "absolute"
                        , Html.Attributes.style "width" "50%"
                        , Html.Attributes.style "height" "10%"
                        , Html.Attributes.style "zIndex" "1"
                        , Html.Attributes.style "cursor" "pointer"
                        , Html.Attributes.style "top" "90%"
                        , Html.Attributes.style "right" "0%"
                        ] 
                        [   if model.seat5.conversation == 0 then
                                Html.text "Hello."
                            else if model.seat5.conversation == 1 then
                                Html.text "Bestellung kommt sofort"
                            else if model.seat5.conversation == 2 then
                                Html.text "Lassen sie es sich schmecken"
                            else 
                                Html.text "??"
                        ]
                    ]
                ]
            else
                Html.text ""
        , if model.timeChoosen == False then
            div 
            [ Html.Attributes.class "overlay"]
            [div [Html.Attributes.class "display-text"] 
                [ Html.text ("Wie lange möchten sie lernen?") ]
            ,input  [ placeholder "Bitte geben sie an wie lange sie lernen wollen."
                    , value model.userInput
                    , onInput GetInput 
                    , Html.Attributes.class "input-field"
                    ] []
            ,div [Html.Attributes.class "display-text"] 
                [ Html.text ("Was möchten sie heute lernen?") ]
            ,input  [ placeholder "Bitte geben sie an was sie lernen wollen."
                    , value model.userInput2
                    , onInput GetInput2 
                    , Html.Attributes.class "input-field"
                    ] []
            , div [Html.Attributes.class "display-text"] 
                [ Html.text ("Ich möchte für " ++ model.userInput ++" Minuten " ++ model.userInput2 ++ " lernen") ]
            , button    [ Html.Attributes.class "confirm-button"
                        , Html.Events.onClick SwitchOverlay 
                        , if checkForInt model.userInput then Html.Attributes.style "background-color" "rgb(0, 255, 0)" else Html.Attributes.style "background-color" "rgb(255, 0, 0)"
                        , disabled (not (checkForInt model.userInput))
                        ] 
                        [ if checkForInt model.userInput then Html.text "Confirm" else Html.text "Bitte richtig eingeben!" ]
            ]
        else
            div 
            [ Html.Attributes.class "overlay"]
            [ div [Html.Attributes.class "display-text"] 
                [ Html.text ("Sie haben noch " ++ String.fromInt model.time ++" Minuten bis zum ersten Besuch") ]
            , button    [ Html.Attributes.class "confirm-button"
                        , Html.Events.onClick SwitchOverlay 
                        ] 
                        [ Html.text "Ich brauche eine Pause." ]
            ]
        ,if Array.length model.daten >= 2 then
            div [ Html.Attributes.class "overlay2" ]
            [ Svg.svg
                [ Svg.Attributes.width <| String.fromInt (Array.length model.daten * 60 + 100) ++ "px"
                , Svg.Attributes.height "100%"
                , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt (Array.length model.daten * 60 + 20) ++ " " ++ String.fromFloat ((List.maximum (Array.toList model.daten) |> Maybe.withDefault 0) + 60))
                ] 
                (drawBars model.daten (Array.toList model.arbeiten) model)
            ]
        else 
            Html.text ""
        ]


-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }