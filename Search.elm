module Main exposing (..)

import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import RemoteData


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { optionList : Dict.Dict String String
    , selectedOptions : List String
    }


initialModel =
    { optionList = Dict.empty
    , selectedOptions = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getOptions )



-- UPDATE


type Msg
    = MorePlease
    | NewOptions (Result Http.Error (List (List String)))
    | AddOption String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getOptions )

        NewOptions (Ok newOptions) ->
            let
                dict =
                    List.filterMap toTuple newOptions |> Dict.fromList
            in
            ( { model | optionList = dict }, Cmd.none )

        NewOptions (Err err) ->
            ( model, Cmd.none )

        AddOption opt ->
            ( { model | selectedOptions = addOption model opt }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [ text "Search" ]
        , select [ onInput AddOption ]
            (List.map mkOption <| Dict.keys model.optionList)
        , div [] [ text (toString model.selectedOptions) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getOptions : Cmd Msg
getOptions =
    let
        foo =
            Debug.log "getting options" 1

        url =
            "https://www.imicrobe.us/sample/search_params.json"

        decoder =
            Decode.list (Decode.list Decode.string)

        request =
            -- Http.get url decoder
            Http.get url decoder
    in
    Http.send NewOptions request


toTuple xs =
    case xs of
        x :: y :: [] ->
            Just ( x, y )

        _ ->
            Nothing


mkOption s =
    Html.option [] [ Html.text s ]


addOption model opt =
    case List.member opt model.selectedOptions of
        True ->
            model.selectedOptions

        _ ->
            model.selectedOptions ++ [ opt ]
