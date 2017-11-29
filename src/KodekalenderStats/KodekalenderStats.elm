module KodekalenderStats exposing (Model, Msg, getAllStats, initModel, subscriptions, update, view)

import Html exposing (..)
import Http
import Json.Decode exposing (at, int, string)
import Time exposing (Time, every, second)


apiUrl : String
apiUrl =
    "https://api.graph.cool/simple/v1/cjagkk5vt3gd9010643np1udd"


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, getAllStats )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    every (30 * second) FetchStats


initModel : Model
initModel =
    { numberOfUsers = Nothing
    , numberOfSolvedSolutions = Nothing
    , errorMessage = Nothing
    }


type alias Model =
    { numberOfUsers : Maybe Int
    , numberOfSolvedSolutions : Maybe Int
    , errorMessage : Maybe String
    }


type Msg
    = NumberOfUsers (Result Http.Error Int)
    | NumberOfSolvedSolutions (Result Http.Error Int)
    | FetchStats Time


getAllStats : Cmd Msg
getAllStats =
    [ getNumberOfUsers, getNumberOfSolvedSolutions ]
        |> Cmd.batch


getNumberOfUsers : Cmd Msg
getNumberOfUsers =
    graphQLPost NumberOfUsers
        "_allUsersMeta(filter: {role: USER}) { count __typename }"
        (at [ "data", "_allUsersMeta", "count" ] int)


getNumberOfSolvedSolutions : Cmd Msg
getNumberOfSolvedSolutions =
    graphQLPost NumberOfSolvedSolutions
        "_allSolutionsMeta(filter: {solved: true}) { count }"
        (at [ "data", "_allSolutionsMeta", "count" ] int)


graphQLPost msg query decoder =
    let
        body =
            """
            {
             "operationName" : null,
             "variables" : {},
             "query" : "{""" ++ query ++ """}"
            }
            """
    in
    Http.post apiUrl (Http.stringBody "application/json" body) decoder
        |> Http.send msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NumberOfUsers (Ok n) ->
            ( { model | numberOfUsers = Just n }, Cmd.none )

        NumberOfUsers (Err error) ->
            ( { model | errorMessage = Just (toString error) }, Cmd.none )

        NumberOfSolvedSolutions (Ok n) ->
            ( { model | numberOfSolvedSolutions = Just n }, Cmd.none )

        NumberOfSolvedSolutions (Err error) ->
            ( { model | errorMessage = Just (toString error) }, Cmd.none )

        FetchStats _ ->
            ( model, getAllStats )


view : Model -> Html Msg
view model =
    let
        numberField selector label =
            selector model
                |> Maybe.map toString
                |> Maybe.withDefault "Laster…"
                |> (\t -> tr [] [ td [] [ text (label ++ ":") ], td [] [ text t ] ])
    in
    div []
        [ h2 [] [ text "Knowits Kodekalender" ]
        , table []
            [ numberField .numberOfUsers "Antall brukere"
            , numberField .numberOfSolvedSolutions "Antall løste oppgaver"
            ]
        , p [] [ text "julekalender.knowit.no" ]
        ]
