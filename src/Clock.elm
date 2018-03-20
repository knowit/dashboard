module Clock exposing (Model, Msg, initModel, subscriptions, update, view)

import Date exposing (..)
import DateFormatting exposing (viewTime)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html, program)
import Style exposing (..)
import Style.Font as Font
import Time exposing (Time, every, second)
import Time.Format exposing (format)


type Styles
    = TimeLabel


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet [ Style.style TimeLabel [ Font.size 50 ] ]


main : Program Never Model Msg
main =
    program
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
    every Time.second UpdateNow


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
    Element.layout stylesheet <|
        (model
            |> Maybe.map (viewTime >> String.toUpper)
            |> Maybe.withDefault "Loading…"
            |> (\t -> el TimeLabel [ alignRight ] (text t))
        )
