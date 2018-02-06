module Clock exposing (Model, Msg, initModel, subscriptions, update, view)

import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html, program)
import Style exposing (..)
import Style.Border as Border
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
    Element.layout stylesheet <|
        (model
            |> Maybe.map (format "%H:%M:%S")
            |> Maybe.withDefault "Loading…"
            |> (\t -> el TimeLabel [ center ] (text t))
        )
