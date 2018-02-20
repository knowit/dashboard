module Kantinemeny exposing (Model, Msg, getMenu, initModel, subscriptions, update, view)

import Html exposing (..)
import Http
import Json.Decode as Decode
import Time exposing (Time, every, minute)


main : Program Never Model Msg
main =
    program
        { init = ( Nothing, getMenu )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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


view : Model -> Html Msg
view model =
    case model of
        Just menu ->
            div [] [ h1 [] [ text "Dagens kantinemeny" ], ul [] <| List.map viewMenuItem menu ]

        Nothing ->
            span [] [ text "Loading…" ]


viewMenuItem : MenuItem -> Html m
viewMenuItem menuItem =
    p [] [ menuItem.name ++ " (" ++ menuItem.price ++ ")" |> text ]
