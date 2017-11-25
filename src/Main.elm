module Main exposing (..)

import Html exposing (..)
import Date exposing (Date)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Time exposing (every, second, Time)


main : Program Never Model Msg
main =
    let
        initModel =
            { now = Nothing
            , departures = Nothing
            , errorMessage = Nothing
            }
    in
        Html.program
            { init = ( initModel, getDepartures Heimdalsgata )
            , view = view
            , update = update
            , subscriptions = subscriptions
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every (10 * second) RefreshDepartures
        , every second UpdateNow
        ]


type Stop
    = Heimdalsgata


type alias Model =
    { now : Maybe Time
    , departures : Maybe (List Departure)
    , errorMessage : Maybe String
    }


type alias DepartureWithTimeDelta =
    { departure : Departure, timeDelta : Time }


type Msg
    = DeparturesResponse (Result Http.Error (List Departure))
    | RefreshDepartures Time
    | UpdateNow Time


type alias Departure =
    { destinationName : String
    , expectedArrivalTime : Date
    , lineRef : String
    , lineColour : String
    }


getDepartures : Stop -> Cmd Msg
getDepartures stop =
    let
        stopId stop =
            case stop of
                Heimdalsgata ->
                    "3010531"

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
            |> Http.send DeparturesResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeparturesResponse (Ok departures) ->
            ( { model | departures = Just departures, errorMessage = Nothing }, Cmd.none )

        DeparturesResponse (Err error) ->
            ( { model | errorMessage = Just (toString error) }, Cmd.none )

        RefreshDepartures _ ->
            ( model, getDepartures Heimdalsgata )

        UpdateNow now ->
            ( { model | now = Just now }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.errorMessage of
        Just string ->
            div []
                [ h1 [] [ text "Error" ]
                , p [] [ text "Something went wrong:" ]
                , p [] [ text string ]
                ]

        Nothing ->
            case model.now of
                Just now ->
                    case model.departures of
                        Just departures ->
                            departures
                                |> List.map (withTimeDelta now)
                                |> hideDeparturesInThePast
                                |> List.sortBy .timeDelta
                                |> List.take 10
                                |> List.map viewDeparture
                                |> table []

                        Nothing ->
                            div [] []

                Nothing ->
                    div [] []


hideDeparturesInThePast : List DepartureWithTimeDelta -> List DepartureWithTimeDelta
hideDeparturesInThePast =
    List.filter (\t -> t.timeDelta > 0)


withTimeDelta : Time -> Departure -> DepartureWithTimeDelta
withTimeDelta time departure =
    { departure = departure
    , timeDelta = (Date.toTime departure.expectedArrivalTime) - time
    }


viewDeparture : DepartureWithTimeDelta -> Html Msg
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

                -- Unknown
                _ ->
                    ""
    in
        tr []
            [ td [] [ text transportTypeSymbol ]
            , td [] [ text departure.lineRef ]
            , td [] [ text departure.destinationName ]
            , td [] [ text (formatTimedelta departureWithTimeDelta.timeDelta) ]
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
        (if hours > 0 then
            [ hours, minutes, seconds ]
         else
            [ minutes, seconds ]
        )
            |> List.map toString
            |> List.map (String.padLeft 2 '0')
            |> String.join ":"
