module RuterMonitor exposing (Model, Msg, getAllDepartures, initModel, subscriptions, update, view)

import Color exposing (..)
import Date exposing (Date)
import Element exposing (..)
import Element.Attributes exposing (..)
import EveryDict
import Html exposing (Html, program)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Style exposing (..)
import Style.Font as Font
import Time exposing (Time, every, second)


main : Program Never Model Msg
main =
    program
        { init = ( initModel, getAllDepartures )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Styles
    = NoStyle
    | DepartureHeading


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ Style.style NoStyle []
        , Style.style DepartureHeading
            [ Font.size 40
            ]
        ]


initModel : Model
initModel =
    { now = Nothing
    , departures = EveryDict.empty
    , errorMessage = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every (10 * second) RefreshDepartures
        , every second UpdateNow
        ]


type Stop
    = Heimdalsgata
    | Gronland


type alias Model =
    { now : Maybe Time
    , departures : EveryDict.EveryDict Stop (List Departure)
    , errorMessage : Maybe String
    }


type alias DepartureWithTimeDelta =
    { departure : Departure, timeDelta : Time }


type Msg
    = DeparturesResponse Stop (Result Http.Error (List Departure))
    | RefreshDepartures Time
    | UpdateNow Time


type alias Departure =
    { destinationName : String
    , expectedArrivalTime : Date
    , lineRef : String
    , lineColour : String
    }


getAllDepartures : Cmd Msg
getAllDepartures =
    [ Heimdalsgata, Gronland ]
        |> List.map getDeparturesForStop
        |> Cmd.batch


getDeparturesForStop : Stop -> Cmd Msg
getDeparturesForStop stop =
    let
        stopId stop =
            case stop of
                Heimdalsgata ->
                    "3010531"

                Gronland ->
                    "3010610"

        url =
            "https://reisapi.ruter.no/stopvisit/getdepartures/"
                ++ stopId stop

        date =
            let
                convert raw =
                    case Date.fromString raw of
                        Ok date ->
                            Decode.succeed date

                        Err error ->
                            Decode.fail error
            in
            Decode.string
                |> Decode.andThen convert

        decodeDeparture =
            decode Departure
                |> requiredAt [ "MonitoredVehicleJourney", "DestinationName" ] Decode.string
                |> requiredAt [ "MonitoredVehicleJourney", "MonitoredCall", "ExpectedArrivalTime" ] date
                |> requiredAt [ "MonitoredVehicleJourney", "LineRef" ] Decode.string
                |> requiredAt [ "Extensions", "LineColour" ] Decode.string
    in
    Decode.list decodeDeparture
        |> Http.get url
        |> Http.send (DeparturesResponse stop)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeparturesResponse stop (Ok departures) ->
            ( { model
                | departures =
                    EveryDict.insert stop departures model.departures
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        DeparturesResponse stop (Err error) ->
            ( { model | errorMessage = Just (toString error) }, Cmd.none )

        RefreshDepartures _ ->
            ( model, getAllDepartures )

        UpdateNow now ->
            ( { model | now = Just now }, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout stylesheet <|
        case model.errorMessage of
            Just string ->
                row NoStyle
                    []
                    [ h1 NoStyle [] (text "Error")
                    , el NoStyle [] (text "Something went wrong:")
                    , el NoStyle [] (text string)
                    ]

            Nothing ->
                case model.now of
                    Just now ->
                        column NoStyle
                            []
                            (model.departures
                                |> EveryDict.toList
                                |> List.map
                                    (\( stop, departures ) ->
                                        viewDepartures stop departures now
                                    )
                            )

                    Nothing ->
                        empty


viewDepartures stop departures now =
    let
        stopLabel =
            case stop of
                Heimdalsgata ->
                    "Heimdalsgata"

                Gronland ->
                    "Grønland t-bane"
    in
    departures
        |> List.map (withTimeDelta now)
        |> hideDeparturesInThePast
        |> List.sortBy .timeDelta
        |> List.take 6
        |> List.map viewDeparture
        |> List.singleton
        |> table NoStyle []
        |> (\t -> column NoStyle [ padding 10 ] [ h2 DepartureHeading [] (text stopLabel), t ])


hideDeparturesInThePast : List DepartureWithTimeDelta -> List DepartureWithTimeDelta
hideDeparturesInThePast =
    List.filter (\t -> t.timeDelta > Time.minute)


withTimeDelta : Time -> Departure -> DepartureWithTimeDelta
withTimeDelta time departure =
    { departure = departure
    , timeDelta = Date.toTime departure.expectedArrivalTime - time
    }


viewDeparture departureWithTimeDelta =
    let
        departure =
            departureWithTimeDelta.departure

        transportTypeSymbol =
            case departure.lineColour of
                -- Bus
                "E60000" ->
                    "🚌"

                -- Tram
                "0B91EF" ->
                    "🚋"

                -- Subway
                "EC700C" ->
                    "🚇"

                -- Unknown
                _ ->
                    ""

        col content =
            column NoStyle [ padding 5, spacing 1 ] [ text content ]
    in
    -- TODO Use grid here to align shit
    row NoStyle
        []
        [ col transportTypeSymbol
        , col departure.lineRef
        , col departure.destinationName
        , col (formatTimedelta departureWithTimeDelta.timeDelta)
        ]


formatTimedelta : Time -> String
formatTimedelta timeDelta =
    let
        hours =
            timeDelta
                |> Time.inHours
                |> floor

        afterHours =
            timeDelta - (toFloat hours * Time.hour)

        minutes =
            afterHours
                |> Time.inMinutes
                |> floor

        afterMinutes =
            afterHours - (toFloat minutes * Time.minute)

        seconds =
            afterMinutes
                |> Time.inSeconds
                |> floor
    in
    if hours > 0 then
        toString hours ++ " t, " ++ toString minutes ++ " min"
    else
        toString minutes
            ++ " min"
