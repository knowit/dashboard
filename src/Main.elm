module Main exposing (..)

import Html exposing (..)
import Clock as C
import RuterMonitor as R


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, R.getAllDepartures |> Cmd.map RuterMonitorMsg )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ C.subscriptions model.clock |> Sub.map ClockMsg
        , R.subscriptions model.ruterMonitor |> Sub.map RuterMonitorMsg
        ]


initModel : Model
initModel =
    { clock = C.initModel
    , ruterMonitor = R.initModel
    }


type alias Model =
    { clock : C.Model
    , ruterMonitor : R.Model
    }


type Msg
    = ClockMsg C.Msg
    | RuterMonitorMsg R.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClockMsg clockMsg ->
            C.update clockMsg model.clock
                |> \( newModel, cmd ) ->
                    ( { model | clock = newModel }, Cmd.map ClockMsg cmd )

        RuterMonitorMsg ruterMonitorMsg ->
            R.update ruterMonitorMsg model.ruterMonitor
                |> \( newModel, cmd ) ->
                    ( { model | ruterMonitor = newModel }, Cmd.map RuterMonitorMsg cmd )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Dashboard" ]
        , C.view model.clock
            |> Html.map ClockMsg
        , R.view model.ruterMonitor
            |> Html.map RuterMonitorMsg
        ]
