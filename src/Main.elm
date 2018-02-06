module Main exposing (..)

import Clock as C
import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html, program)
import RuterMonitor as R
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color


main : Program Never Model Msg
main =
    program
        { init = ( initModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Styles
    = NoStyle
    | GridStyle
    | CellStyle


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ Style.style NoStyle []
        , Style.style GridStyle []
        , Style.style CellStyle
            [ Color.background gray
            , Color.border black
            , Border.solid
            , Border.rounded 5
            , Border.all 1
            ]
        ]


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
    let
        emptyCell pos =
            cell { start = pos, width = 1, height = 1, content = el NoStyle [] (text "") }
    in
    grid GridStyle
        [ padding 10 ]
        { columns = [ percent 20, percent 20, percent 20, percent 20, percent 20 ]
        , rows = [ percent 20, percent 20, percent 20, percent 20, percent 20 ]
        , cells =
            [ cell
                { start = ( 0, 0 )
                , width = 1
                , height = 5
                , content = column CellStyle [] [ R.view model.ruterMonitor |> Html.map RuterMonitorMsg |> html ]
                }
            , emptyCell ( 1, 0 )
            , emptyCell ( 2, 0 )
            , emptyCell ( 3, 0 )
            , cell
                { start = ( 4, 0 )
                , width = 1
                , height = 1
                , content = column CellStyle [] [ C.view model.clock |> Html.map ClockMsg |> html ]
                }
            ]
        }
        |> Element.layout stylesheet
