module Main exposing (..)

import Html exposing (..)
import Http
import Json.Decode exposing (list, string)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    String



--     { options : List String
--     , criteria : List String
--     , results : List String
--     }
-- UPDATE


type Msg
    = Search String



--     | NewOptions (Result Http.Error String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Search criteria ->
            model



--         NewOptions options ->
--             { model | options = options }
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ Html.text model ]



-- INIT


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )



-- getOptions : String -> Cmd Msg
-- getOptions =
--     let
--         url =
--             "https://www.imicrobe.us/sample/search_params.json"
--
--         request =
--             Http.get url (list string)
--     in
--     Http.send NewOptions request
