module Clock exposing (Model, Msg, update, view, subscriptions, initModel)

import Html exposing (..)


type Msg
    = NoOp


type alias Model =
    MeetingRoom


type alias MeetingRoom =
    { floor : Floor
    , number : Int
    , name : RoomName
    , calendarEmail : EmailAddress
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
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
