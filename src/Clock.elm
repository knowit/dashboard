module Clock exposing (Model, Msg, initModel, subscriptions, update, view)

import Date exposing (..)
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


norwegianLabelForDay : Day -> String
norwegianLabelForDay day =
    case day of
        Mon ->
            "Mandag"

        Tue ->
            "Tirsdag"

        Wed ->
            "Onsdag"

        Thu ->
            "Torsdag"

        Fri ->
            "Fredag"

        Sat ->
            "Lørdag"

        Sun ->
            "Søndag"


norwegianLabelForMonth : Month -> String
norwegianLabelForMonth month =
    case month of
        Jan ->
            "januar"

        Feb ->
            "februar"

        Mar ->
            "mars"

        Apr ->
            "april"

        May ->
            "mai"

        Jun ->
            "juni"

        Jul ->
            "juli"

        Aug ->
            "august"

        Sep ->
            "september"

        Oct ->
            "oktober"

        Nov ->
            "november"

        Dec ->
            "desember"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNow now ->
            ( Just now, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout stylesheet <|
        (model
            |> Maybe.map viewTime
            |> Maybe.withDefault "Loading…"
            |> (\t -> el TimeLabel [ alignRight ] (text t))
        )


viewTime : Time.Time -> String
viewTime time =
    let
        clock =
            format "%H:%M" time

        asDate =
            Date.fromTime time

        date =
            (norwegianLabelForDay <| dayOfWeek asDate)
                ++ " "
                ++ (toString <| day asDate)
                ++ ". "
                ++ (norwegianLabelForMonth <| month asDate)
    in
    date
        ++ " "
        ++ clock
        |> String.toUpper
