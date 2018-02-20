module Kantinemeny exposing (Model, Msg, getMenu, initModel, subscriptions, update, view)

import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (program)
import Http
import Json.Decode as Decode
import Style exposing (..)
import Style.Font as Font
import Time exposing (Time, every, minute, second)


main : Program Never Model Msg
main =
    program
        { init = ( Nothing, getMenu )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Styles
    = Header
    | NoStyle


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet [ Style.style Header [ Font.size 40 ] ]


initModel : Model
initModel =
    Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    every minute GetMenu


type alias Model =
    Maybe Menu


type Msg
    = GetMenu Time.Time
    | MenuResult (Result Http.Error Menu)


type alias Menu =
    List MenuItem


type alias MenuItem =
    { name : String
    , price : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetMenu _ ->
            ( model, getMenu )

        MenuResult (Ok menu) ->
            ( Just menu, Cmd.none )

        MenuResult (Err _) ->
            ( model, Cmd.none )


menuDecoder : Decode.Decoder Menu
menuDecoder =
    Decode.list
        (Decode.map2 MenuItem
            (Decode.field "name" Decode.string)
            (Decode.field "price" Decode.string)
        )


getMenu : Cmd Msg
getMenu =
    Http.get "https://us-central1-knowit-cloud-9b7de.cloudfunctions.net/getMenuForDay" menuDecoder
        |> Http.send MenuResult


view : Model -> Html.Html Msg
view model =
    Element.layout stylesheet <|
        case model of
            Just menu ->
                column NoStyle
                    [ padding 10 ]
                    [ el Header [] (text "Dagens kantinemeny")
                    , column NoStyle [ padding 10 ] <| List.map viewMenuItem menu
                    ]

            Nothing ->
                el NoStyle [] (text "Loading…")


viewMenuItem : MenuItem -> Element Styles variation msg
viewMenuItem menuItem =
    el NoStyle [ padding 10 ] (menuItem.name ++ " (" ++ menuItem.price ++ ")" |> text)
