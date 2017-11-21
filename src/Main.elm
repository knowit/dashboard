module Main exposing (..)

import Html exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( "", Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    String


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Sundtkvartalet Dashboard" ]
        , p [] [ text "Her skal vi vise frem nyttig info for folk som jobber i Sundkvartalet." ]
        ]
