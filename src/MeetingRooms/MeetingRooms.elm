module Clock exposing (Model, Msg, update, view, subscriptions, initModel)

import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import List exposing (sortBy)
import Date exposing (Date)


type Msg
    = RoomsFreeBusyResponse (Result Http.Error (List RoomFreeBusy))


type alias Model =
    { rooms : List RoomFreeBusy
    , error : Maybe String
    }


type alias MeetingRoom =
    { floor : Floor
    , number : Int
    , name : RoomName
    , calendarEmail : EmailAddress
    }


type alias RoomFreeBusy =
    { roomCode : String
    , roomName : String
    , isBusy : Bool
    }


type Floor
    = Sundt4th
    | Sundt5th


type alias RoomName =
    String


type alias EmailAddress =
    String


midten : MeetingRoom
midten =
    { floor = Sundt4th
    , number = 16
    , name = "Midten"
    , calendarEmail = "knowit.no_2d3632363431323237383036@resource.calendar.google.com"
    }


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, getRoomsFreeBusy )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModel : Model
initModel =
    { rooms = [], error = Nothing }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getRoomsFreeBusy : Cmd Msg
getRoomsFreeBusy =
    let
        url =
            -- Only works on Knowit network or via Knowit VPN
            -- "/src/MeetingRooms/example_rooms.json"
            "http://10.205.0.5:4422/rooms"

        decodeResult : Result x a -> Decoder a
        decodeResult result =
            case result of
                Ok value ->
                    Decode.succeed value

                Err error ->
                    Decode.fail ("Decode failed: " ++ (toString error))

        decodeDate : Decoder Date
        decodeDate =
            Decode.string
                |> Decode.map Date.fromString
                |> Decode.andThen decodeResult

        decodeRoom =
            decode RoomFreeBusy
                |> required "roomCode" Decode.string
                |> required "roomName" Decode.string
                |> required "isBusy" Decode.bool
    in
        Decode.list decodeRoom
            |> Http.get url
            |> Http.send RoomsFreeBusyResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        sortRooms : List RoomFreeBusy -> List RoomFreeBusy
        sortRooms rooms =
            sortBy .roomCode rooms
    in
        case msg of
            RoomsFreeBusyResponse (Ok roomsFreeBusy) ->
                ( { model | rooms = sortRooms roomsFreeBusy }, Cmd.none )

            RoomsFreeBusyResponse (Err error) ->
                ( { model | error = Just (toString error) }, Cmd.none )


viewOld : MeetingRoom -> Html Msg
viewOld model =
    text <|
        (floorToString model.floor)
            ++ ", rom "
            ++ (toString model.number)
            ++ ": "
            ++ model.name


view : Model -> Html Msg
view model =
    let
        ( busyRooms, freeRooms ) =
            List.partition .isBusy model.rooms
    in
        div []
            [ h4 [] [ text ("Error : " ++ (Maybe.withDefault "" model.error)) ]
            , h5 [] [ text "Free Rooms:" ]
            , div []
                (List.map viewRoomFreeBusy freeRooms)
            , h5 [] [ text "Busy Rooms:" ]
            , div []
                (List.map viewRoomFreeBusy busyRooms)
            ]


viewRoomFreeBusy : RoomFreeBusy -> Html Msg
viewRoomFreeBusy room =
    p []
        [ text
            (room.roomCode
                ++ " "
                ++ room.roomName
            )
        ]


floorToString : Floor -> String
floorToString floor =
    case floor of
        Sundt4th ->
            "4. etasje"

        Sundt5th ->
            "5. etasje"
