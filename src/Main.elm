module Main exposing (..)

import Clock as C
import Html exposing (..)
import Html.Attributes exposing (class, src)
import KodekalenderStats as K
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
    , K.getAllStats |> Cmd.map KalenderStatsMsg
    ]
        |> Cmd.batch


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ C.subscriptions model.clock |> Sub.map ClockMsg
        , R.subscriptions model.ruterMonitor |> Sub.map RuterMonitorMsg
        , K.subscriptions model.kalenderStats |> Sub.map KalenderStatsMsg
        ]


initModel : Model
initModel =
    { clock = C.initModel
    , ruterMonitor = R.initModel
    , kalenderStats = K.initModel
    }


type alias Model =
    { clock : C.Model
    , ruterMonitor : R.Model
    , kalenderStats : K.Model
    }


type Msg
    = ClockMsg C.Msg
    | RuterMonitorMsg R.Msg
    | KalenderStatsMsg K.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClockMsg clockMsg ->
            C.update clockMsg model.clock
                |> wrap (\m -> { model | clock = m }) ClockMsg

        RuterMonitorMsg ruterMonitorMsg ->
            R.update ruterMonitorMsg model.ruterMonitor
                |> wrap (\m -> { model | ruterMonitor = m }) RuterMonitorMsg

        KalenderStatsMsg kalenderStatsmsg ->
            K.update kalenderStatsmsg model.kalenderStats
                |> wrap (\m -> { model | kalenderStats = m }) KalenderStatsMsg


wrap : (b -> c) -> (a -> msg) -> ( b, Cmd a ) -> ( c, Cmd msg )
wrap toModelFunction subType ( newModel, cmd ) =
    ( toModelFunction newModel, Cmd.map subType cmd )


view : Model -> Html Msg
view model =
    div [ class "grid" ]
        [ viewSubmodule model C.view .clock ClockMsg "clock"
        , viewSubmodule model R.view .ruterMonitor RuterMonitorMsg "ruterMonitor"
        , viewSubmodule model K.view .kalenderStats KalenderStatsMsg "kalenderStats"
        , div [ class "christmasTree" ] [ img [ src "https://s0.wp.com/wp-content/mu-plugins/wpcom-smileys/twemoji/2/svg/1f384.svg" ] [] ]
        ]


viewSubmodule : b -> (c -> Html a) -> (b -> c) -> (a -> msg) -> String -> Html msg
viewSubmodule model viewFunction selector subType cssClass =
    section [ class cssClass ] [ viewFunction (selector model) |> Html.map subType ]
