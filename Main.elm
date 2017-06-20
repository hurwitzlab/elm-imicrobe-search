module Main exposing (..)

import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type JsonType
    = StrType String
    | IntType Int
    | FloatType Float
    | ValueType Decode.Value


type alias Model =
    { optionList : Dict.Dict String String
    , selectedOptions : List ( String, String )
    , optionValue : Dict.Dict String String
    , searchResults : WebData (List (Dict.Dict String JsonType))
    }


initialModel =
    { optionList = Dict.empty
    , selectedOptions = []
    , optionValue = Dict.empty
    , searchResults = NotAsked
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getOptions )



-- UPDATE


type Msg
    = MorePlease
    | NewOptions (Result Http.Error (List (List String)))
    | AddOption String
    | RemoveOption String
    | UpdateOptionValue String String
    | Search
    | UpdateSearchResults (WebData (List (Dict.Dict String JsonType)))


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
            ( { model
                | selectedOptions = addOption model opt
                , optionList = Dict.remove opt model.optionList
              }
            , Cmd.none
            )

        RemoveOption opt ->
            ( { model | selectedOptions = rmOption model.selectedOptions opt }, Cmd.none )

        UpdateOptionValue opt val ->
            ( { model | optionValue = Dict.insert opt val model.optionValue }, Cmd.none )

        Search ->
            ( model, doSearch model.optionValue )

        UpdateSearchResults response ->
            ( { model | searchResults = response }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style [ ( "width", "100%" ) ] ]
        [ h1 [] [ text "Search" ]
        , div [ style [ ( "text-align", "center" ) ] ]
            [ text "Select: "
            , select [ onInput AddOption ]
                (List.map mkOption <| "-- Select --" :: Dict.keys model.optionList)
            ]
        , div [] [ mkOptionTable model.selectedOptions ]
        , div [] [ showSearchResults model.searchResults ]
        , div [] [ text (toString model.optionValue) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getOptions : Cmd Msg
getOptions =
    let
        url =
            "https://www.imicrobe.us/sample/search_params.json"

        decoder =
            Decode.list (Decode.list Decode.string)

        request =
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


addOption : Model -> String -> List ( String, String )
addOption model opt =
    let
        isAlreadySelected =
            List.member opt (List.map Tuple.first model.selectedOptions)
    in
    case ( Dict.get opt model.optionList, isAlreadySelected ) of
        ( Just dataType, False ) ->
            model.selectedOptions ++ [ ( opt, dataType ) ]

        ( _, _ ) ->
            model.selectedOptions


rmOption : List ( String, String ) -> String -> List ( String, String )
rmOption optionList optToRemove =
    List.filter (\( k, v ) -> k /= optToRemove) optionList


mkOptionTable options =
    let
        rows =
            List.map mkRow options

        searchButtonRow =
            [ tr []
                [ td [ colspan 4, style [ ( "text-align", "center" ) ] ]
                    [ button [ onClick Search ] [ text "Search" ] ]
                ]
            ]
    in
    case rows of
        [] ->
            text ""

        _ ->
            table [ style [ ( "width", "100%" ) ] ]
                (rows ++ searchButtonRow)


mkRow : ( String, String ) -> Html Msg
mkRow ( optionName, dataType ) =
    let
        title =
            [ th [] [ text optionName ] ]

        minName =
            "min__" ++ optionName

        maxName =
            "max__" ++ optionName

        el =
            case dataType of
                "number" ->
                    [ td [ onInput (UpdateOptionValue minName) ]
                        [ text "Min: "
                        , input [ type_ "text", placeholder "min", name minName ] []
                        ]
                    , td [ onInput (UpdateOptionValue maxName) ]
                        [ text "Max: "
                        , input [ type_ "text", placeholder "max", name maxName ] []
                        ]
                    ]

                _ ->
                    [ td [ onInput (UpdateOptionValue optionName) ]
                        [ input [ type_ "text", placeholder dataType ] [] ]
                    , td [] []
                    ]

        buttons =
            [ td [] [ button [ onClick (RemoveOption optionName) ] [ text "Remove" ] ]
            ]
    in
    tr [] (title ++ el ++ buttons)


showSearchResults results =
    case results of
        NotAsked ->
            div [ style [ ( "text-align", "center" ) ] ]
                [ text "Choose you must"
                ]

        Loading ->
            text "Loading"

        Failure e ->
            text (toString e)

        Success data ->
            searchResultsTable data


searchResultsTable results =
    case results of
        [] ->
            text "No results"

        _ ->
            div [] (List.map searchResultRow results)


searchResultRow result =
    let
        sampleName =
            case Dict.get "specimen__sample_name" result of
                Just (StrType name) ->
                    name

                _ ->
                    "NA"
    in
    div [] [ text sampleName ]


doSearch options =
    let
        url =
            "https://www.imicrobe.us/sample/search_results.json"

        dictList =
            Dict.toList options

        encoded =
            Encode.object (List.map (\( k, v ) -> ( k, Encode.string v )) dictList)

        body =
            Http.jsonBody encoded

        decoderDict =
            [ Decode.string
                |> Decode.map StrType
            , Decode.int
                |> Decode.map IntType
            , Decode.float
                |> Decode.map FloatType
            , Decode.value
                |> Decode.map ValueType
            ]
                |> Decode.oneOf
                |> Decode.dict

        decoder =
            Decode.at [ "samples" ] (Decode.list decoderDict)
    in
    Http.post url body decoder
        |> RemoteData.sendRequest
        |> Cmd.map UpdateSearchResults
