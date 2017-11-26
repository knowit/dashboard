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
                |> wrap (\m -> { model | clock = m }) ClockMsg

        RuterMonitorMsg ruterMonitorMsg ->
            R.update ruterMonitorMsg model.ruterMonitor
                |> wrap (\m -> { model | ruterMonitor = m }) RuterMonitorMsg


wrap : (b -> c) -> (a -> msg) -> ( b, Cmd a ) -> ( c, Cmd msg )
wrap toModelFunction subType ( newModel, cmd ) =
    ( toModelFunction newModel, Cmd.map subType cmd )


view : Model -> Html Msg
view model =
    div []
        [ C.view model.clock
            |> Html.map ClockMsg
        , R.view model.ruterMonitor
            |> Html.map RuterMonitorMsg
        ]
