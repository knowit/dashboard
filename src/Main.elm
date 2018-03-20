module Main exposing (..)

import Clock as C
import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html, program)
import Kantinemeny as K
import RuterMonitor as R
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


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
            [ Color.background <| Color.rgb 208 207 205
            ]
        ]


initialCmd : Cmd Msg
initialCmd =
    [ R.getAllDepartures |> Cmd.map RuterMonitorMsg
    , K.getMenu |> Cmd.map KantinemenyMsg
    ]
        |> Cmd.batch


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ C.subscriptions model.clock |> Sub.map ClockMsg
        , R.subscriptions model.ruterMonitor |> Sub.map RuterMonitorMsg
        , K.subscriptions model.kantinemeny |> Sub.map KantinemenyMsg
        ]


initModel : Model
initModel =
    { clock = C.initModel
    , ruterMonitor = R.initModel
    , kantinemeny = K.initModel
    }


type alias Model =
    { clock : C.Model
    , ruterMonitor : R.Model
    , kantinemeny : K.Model
    }


type Msg
    = ClockMsg C.Msg
    | RuterMonitorMsg R.Msg
    | KantinemenyMsg K.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClockMsg clockMsg ->
            C.update clockMsg model.clock
                |> wrap (\m -> { model | clock = m }) ClockMsg

        RuterMonitorMsg ruterMonitorMsg ->
            R.update ruterMonitorMsg model.ruterMonitor
                |> wrap (\m -> { model | ruterMonitor = m }) RuterMonitorMsg

        KantinemenyMsg kantinemenyMsg ->
            K.update kantinemenyMsg model.kantinemeny
                |> wrap (\m -> { model | kantinemeny = m }) KantinemenyMsg


wrap : (b -> c) -> (a -> msg) -> ( b, Cmd a ) -> ( c, Cmd msg )
wrap toModelFunction subType ( newModel, cmd ) =
    ( toModelFunction newModel, Cmd.map subType cmd )


view : Model -> Html Msg
view model =
    grid GridStyle
        [ paddingLeft 20, paddingRight 20, paddingTop 10, paddingBottom 10, spacing 35 ]
        { columns = [ percent 25, percent 25, percent 25, percent 21 ]
        , rows = [ percent 5, percent 1, percent 40, percent 40 ]
        , cells =
            [ cell
                { start = ( 0, 0 )
                , width = 1
                , height = 1
                , content = image NoStyle [ height (px 50) ] { src = "https://osloelmday.no/images/knowit.svg", caption = "Knowit logo" }
                }
            , cell
                { start = ( 3, 0 )
                , width = 1
                , height = 1
                , content = column NoStyle [] [ C.view model.clock |> Html.map ClockMsg |> html ]
                }
            , cell
                { start = ( 0, 1 )
                , width = 4
                , height = 1
                , content = el NoStyle [] <| html <| Html.hr [] []
                }
            , cell
                { start = ( 0, 2 )
                , width = 2
                , height = 2
                , content = el CellStyle [ padding 100 ] (image NoStyle [] { src = "http://sundtcommander.knowit.no/sundt.svg", caption = "Møteromsoversikt" })
                }
            , cell
                { start = ( 2, 2 )
                , width = 2
                , height = 1
                , content = el CellStyle [] (text "Kalender")
                }
            , cell
                { start = ( 2, 3 )
                , width = 1
                , height = 1
                , content = column CellStyle [] [ K.view model.kantinemeny |> Html.map KantinemenyMsg |> html ]
                }
            , cell
                { start = ( 3, 3 )
                , width = 1
                , height = 1
                , content = column CellStyle [] [ R.view model.ruterMonitor |> Html.map RuterMonitorMsg |> html ]
                }
            ]
        }
        |> Element.layout stylesheet
