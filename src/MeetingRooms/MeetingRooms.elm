module Clock exposing (Model, Msg, update, view, subscriptions, initModel)

import Html exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)


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
            --"http://10.205.0.5:4422/rooms"  -- Need to fix CORS issue
            "/src/MeetingRooms/example_rooms.json"

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
    case msg of
        RoomsFreeBusyResponse (Ok roomsFreeBusy) ->
            ( { model | rooms = roomsFreeBusy }, Cmd.none )

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
    div []
        [ h4 [] [ text ("Error : " ++ (Maybe.withDefault "" model.error)) ]
        , div []
            (List.map viewRoomFreeBusy model.rooms)
        ]


viewRoomFreeBusy : RoomFreeBusy -> Html Msg
viewRoomFreeBusy room =
    p []
        [ text
            (room.roomCode
                ++ " "
                ++ room.roomName
                ++ ": "
                ++ (toString room.isBusy)
            )
        ]


floorToString : Floor -> String
floorToString floor =
    case floor of
        Sundt4th ->
            "4. etasje"

        Sundt5th ->
            "5. etasje"
