module Clock exposing (Model, Msg, update, view, subscriptions, initModel)

import Html exposing (..)
import Time exposing (every, second, Time)
import Time.Format exposing (format)


main : Program Never Model Msg
main =
    Html.program
        { init = ( Nothing, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModel : Model
initModel =
    Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    every second UpdateNow


type alias Model =
    Maybe Time


type Msg
    = UpdateNow Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNow now ->
            ( Just now, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Just time ->
            time
                |> format "%H:%M:%S"
                |> text

        Nothing ->
            text "Loading…"
