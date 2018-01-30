module Clock exposing (Model, Msg, update, view, subscriptions, initModel)

import Html exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)


type Msg
    = RoomsFreeBusyResponse (Result Http.Error (List RoomFreeBusy))


type alias Model =
    MeetingRoom


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
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModel : Model
initModel =
    midten


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getRoomsFreeBusy : Cmd Msg
getRoomsFreeBusy =
    let
        url =
            -- Only works on Knowit network or via Knowit VPN
            "http://10.205.0.5:4422/rooms"

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
        RoomsFreeBusyResponse _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text <|
        (floorToString model.floor)
            ++ ", rom "
            ++ (toString model.number)
            ++ ": "
            ++ model.name


floorToString : Floor -> String
floorToString floor =
    case floor of
        Sundt4th ->
            "4. etasje"

        Sundt5th ->
            "5. etasje"
