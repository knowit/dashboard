module MeetingRooms exposing (Model, Msg, getRoomsAvailability, initModel, subscriptions, update, view)

import Date exposing (Date)
import DateFormatting exposing (viewTime)
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import List exposing (sortBy)
import Time exposing (Time, every, second)


type Msg
    = RoomsAvailabilityResponse (Result Http.Error (List RoomAvailability))
    | Refresh Time


type alias Model =
    { rooms : Maybe (List RoomAvailability)
    , error : Maybe String
    }


type alias MeetingRoom =
    { floor : Floor
    , number : Int
    , name : RoomName
    , calendarEmail : EmailAddress
    }


type alias RoomAvailability =
    { roomCode : String
    , roomName : RoomName
    , isBusy : Bool
    , currentEventEnd : Maybe Date
    , nextEventStart : Maybe Date
    }


type Floor
    = Sundt4th
    | Sundt5th


type alias RoomName =
    String


type alias EmailAddress =
    String


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, getRoomsAvailability )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModel : Model
initModel =
    { rooms = Nothing, error = Nothing }


subscriptions : Model -> Sub Msg
subscriptions model =
    every (30 * second) Refresh


getRoomsAvailability : Cmd Msg
getRoomsAvailability =
    let
        url =
            -- Only works on Knowit network or via Knowit VPN
            -- "/src/example_data/example_rooms.json"
            "http://10.205.0.5:4422/rooms"

        decodeResult : Result x a -> Decoder a
        decodeResult result =
            case result of
                Ok value ->
                    Decode.succeed value

                Err error ->
                    Decode.fail ("Decode failed: " ++ toString error)

        decodeDate : Decoder Date
        decodeDate =
            Decode.string
                |> Decode.map Date.fromString
                |> Decode.andThen decodeResult

        decodeRoom =
            decode RoomAvailability
                |> required "roomCode" Decode.string
                |> required "roomName" Decode.string
                |> required "isBusy" Decode.bool
                |> required "currentEventEnd" (Decode.nullable decodeDate)
                |> required "nextEventStart" (Decode.nullable decodeDate)
    in
    Decode.list decodeRoom
        |> Http.get url
        |> Http.send RoomsAvailabilityResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RoomsAvailabilityResponse (Ok roomsAvailabilities) ->
            ( { model | rooms = Just roomsAvailabilities }, Cmd.none )

        RoomsAvailabilityResponse (Err error) ->
            ( { model | error = Just (toString error) }, Cmd.none )

        Refresh _ ->
            ( model, getRoomsAvailability )


view : Model -> Html Msg
view model =
    case model.rooms of
        Just rooms ->
            let
                ( busyRooms, freeRooms ) =
                    rooms
                        |> sortBy .roomCode
                        |> List.partition .isBusy
            in
            div []
                [ h3 [] [ text "Ledige rom" ]
                , freeRooms
                    |> List.map viewRoomAvailability
                    |> ul []
                , h3 [] [ text "Opptatte rom" ]
                , busyRooms
                    |> List.map viewRoomAvailability
                    |> ul []
                , case model.error of
                    Just errorMsg ->
                        h3 [] [ text ("Error: " ++ errorMsg) ]

                    Nothing ->
                        span [] []
                ]

        Nothing ->
            h3 [] [ text "Loading…" ]


viewRoomAvailability : RoomAvailability -> Html Msg
viewRoomAvailability room =
    let
        availabilityDesc =
            if room.isBusy then
                "Opptatt til "
                    ++ (room.currentEventEnd
                            |> Maybe.map (\d -> viewTime (Date.toTime d) ++ ".")
                            |> Maybe.withDefault "??"
                       )
            else
                "Ledig"
                    ++ (room.nextEventStart
                            |> Maybe.map (\d -> " til " ++ viewTime (Date.toTime d) ++ ".")
                            |> Maybe.withDefault ", uten senere booking."
                       )
    in
    li []
        [ text
            (room.roomCode
                ++ " "
                ++ room.roomName
                ++ ": "
                ++ availabilityDesc
            )
        ]


floorToString : Floor -> String
floorToString floor =
    case floor of
        Sundt4th ->
            "4. etasje"

        Sundt5th ->
            "5. etasje"
