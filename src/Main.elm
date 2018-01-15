module Main exposing (..)

import Clock as C
import Html exposing (..)
import Html.Attributes exposing (class, src)
import RuterMonitor as R


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialCmd : Cmd Msg
initialCmd =
    [ R.getAllDepartures |> Cmd.map RuterMonitorMsg
    ]
        |> Cmd.batch


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
    div [ class "grid" ]
        [ viewSubmodule model C.view .clock ClockMsg "clock"
        , viewSubmodule model R.view .ruterMonitor RuterMonitorMsg "ruterMonitor"
        ]


viewSubmodule : b -> (c -> Html a) -> (b -> c) -> (a -> msg) -> String -> Html msg
viewSubmodule model viewFunction selector subType cssClass =
    section [ class cssClass ] [ viewFunction (selector model) |> Html.map subType ]
