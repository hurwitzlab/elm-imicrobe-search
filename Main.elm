module Main exposing (..)

import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (unique)
import RemoteData exposing (..)
import Set


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
    { optionList : WebData (Dict.Dict String String)
    , restrictedOptionList : Dict.Dict String String
    , restrictedOptionValues : Dict.Dict String (List JsonType)
    , selectedOptions : List ( String, String )
    , optionValue : Dict.Dict String String
    , searchResults : WebData (List (Dict.Dict String JsonType))
    }


initialModel =
    { optionList = NotAsked
    , restrictedOptionList = Dict.empty
    , restrictedOptionValues = Dict.empty
    , selectedOptions = []
    , optionValue = Dict.empty
    , searchResults = NotAsked
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getOptions )


type Msg
    = NewOptions (WebData (Dict.Dict String String))
    | AddOption String
    | RemoveOption String
    | UpdateOptionValue String String
    | Search
    | UpdateSearchResults (WebData (List (Dict.Dict String JsonType)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewOptions response ->
            ( { model | optionList = response }, Cmd.none )

        AddOption opt ->
            ( { model | selectedOptions = addOption model opt }, Cmd.none )

        RemoveOption opt ->
            let
                newOptionValue =
                    rmOptionValue model.optionValue opt
            in
            ( { model
                | selectedOptions = rmOption model.selectedOptions opt
                , optionValue = newOptionValue
              }
            , doSearch newOptionValue
            )

        UpdateOptionValue opt val ->
            ( { model | optionValue = Dict.insert opt val model.optionValue }, Cmd.none )

        Search ->
            ( model, doSearch model.optionValue )

        UpdateSearchResults response ->
            ( { model
                | searchResults = response
                , restrictedOptionList = mkRestrictedOptionList model.optionList response
                , restrictedOptionValues = mkRestrictedOptionValues response
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style [ ( "width", "100%" ) ] ]
        [ h1 [] [ text "Search" ]
        , div [ style [ ( "text-align", "center" ) ] ] (mkOptionSelect model)
        , div [] [ text <| "selectedOptions = " ++ toString model.selectedOptions ]
        , div [] [ mkOptionTable model.selectedOptions ]
        , div [] [ showSearchResults model ]
        , div [] [ text <| "optionValues = " ++ toString model.optionValue ]
        , div [] [ text <| "restrictedOptions = " ++ toString model.restrictedOptionList ]
        , div [] [ text <| "restrictedValues = " ++ toString model.restrictedOptionValues ]
        , div [] [ text <| "searchResults = " ++ toString model.searchResults ]
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
            Decode.dict Decode.string
    in
    Http.get url decoder
        |> RemoteData.sendRequest
        |> Cmd.map NewOptions


mkOptionSelect model =
    case model.optionList of
        NotAsked ->
            [ text "NotAsked" ]

        Loading ->
            [ text "Loading" ]

        Failure e ->
            [ text (toString e) ]

        Success options ->
            let
                first =
                    Html.option [] [ text "-- Select --" ]

                alreadySelected =
                    List.map Tuple.first model.selectedOptions |> Set.fromList

                showKeys =
                    Dict.keys options
                        |> List.filter (\v -> not (Set.member v alreadySelected))

                rest =
                    List.map mkOption showKeys
            in
            [ text "Field: "
            , select [ onInput AddOption ] (first :: rest)
            ]


ucFirst s =
    (String.toUpper <| String.slice 0 1 s) ++ String.slice 1 (String.length s) s


prettyName s =
    let
        parts =
            String.split "__" s

        ( category, name ) =
            case parts of
                first :: rest :: [] ->
                    ( first, rest )

                _ ->
                    ( "NA", String.join "_" parts )

        nameParts =
            String.split "_" name
    in
    ucFirst category ++ ": " ++ String.join " " (List.map ucFirst nameParts)


mkOption s =
    Html.option [ value s ] [ text (prettyName s) ]


addOption model optionName =
    case model.optionList of
        Success dict ->
            case Dict.get optionName dict of
                Just dataType ->
                    model.selectedOptions ++ [ ( optionName, dataType ) ]

                _ ->
                    model.selectedOptions

        _ ->
            model.selectedOptions


rmOption : List ( String, String ) -> String -> List ( String, String )
rmOption optionList optToRemove =
    List.filter (\( k, v ) -> k /= optToRemove) optionList


rmOptionValue : Dict.Dict String String -> String -> Dict.Dict String String
rmOptionValue optionValue optToRemove =
    let
        names =
            Set.fromList [ optToRemove, "min__" ++ optToRemove, "max__" ++ optToRemove ]
    in
    Dict.toList optionValue
        |> List.filter (\( k, v ) -> not (Set.member k names))
        |> Dict.fromList


mkOptionTable options =
    let
        rows =
            List.map mkOptionRow options

        searchButtonRow =
            [ tr []
                [ td [ colspan 4, style [ ( "text-align", "center" ) ] ]
                    [ button [ onClick Search ] [ text "Search" ] ]
                ]
            ]
    in
    case rows of
        [] ->
            text "No options"

        _ ->
            table [ style [ ( "width", "100%" ) ] ]
                (rows ++ searchButtonRow)


mkOptionRow : ( String, String ) -> Html Msg
mkOptionRow ( optionName, dataType ) =
    let
        title =
            [ th [] [ text (prettyName optionName) ] ]

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


showSearchResults model =
    case model.searchResults of
        NotAsked ->
            div [ style [ ( "text-align", "center" ) ] ]
                [ text "Select some fields to search"
                ]

        Loading ->
            text "Loading"

        Failure e ->
            text (toString e)

        Success data ->
            searchResultsTable data model.selectedOptions



-- searchResultsTable :
--     List (Dict.Dict String JsonType) ->
--     List ( String, String ) ->
-- List Html.msg


searchResultsTable results selectedOptions =
    case results of
        [] ->
            text "No results"

        _ ->
            let
                names =
                    List.map Tuple.first selectedOptions

                header =
                    [ tr []
                        (List.map (\s -> th [] [ text s ]) ("name" :: names))
                    ]

                rows =
                    List.map (searchResultRow selectedOptions) results
            in
            table [] (header ++ rows)


searchResultRow selectedOptions result =
    let
        getVal key =
            case Dict.get key result of
                Just a ->
                    toString a

                _ ->
                    "NA"

        mkTd ( key, dataType ) =
            let
                textAlign =
                    case dataType of
                        "number" ->
                            "right"

                        _ ->
                            "left"
            in
            td [ style [ ( "text-align", textAlign ) ] ] [ text (getVal key) ]

        allFields =
            ( "specimen__sample_name", "string" ) :: selectedOptions
    in
    tr []
        (List.map mkTd allFields)


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


mkRestrictedOptionList :
    WebData (Dict.Dict String String)
    -> WebData (List (Dict.Dict String JsonType))
    -> Dict.Dict String String
mkRestrictedOptionList optionList result =
    let
        optionDict =
            case optionList of
                Success optionDict ->
                    optionDict

                _ ->
                    Dict.empty
    in
    case result of
        Success data ->
            let
                keys =
                    List.map Dict.keys data
                        |> List.concat
                        |> List.filter (\v -> v /= "_id")
                        |> unique

                types =
                    List.filterMap (\k -> Dict.get k optionDict) keys
            in
            Dict.fromList (List.map2 (,) keys types)

        _ ->
            Dict.empty


mkRestrictedOptionValues : WebData (List (Dict.Dict String JsonType)) -> Dict.Dict String (List JsonType)
mkRestrictedOptionValues response =
    case response of
        Success data ->
            List.foldl mergeDicts Dict.empty data

        _ ->
            Dict.empty


mergeDicts : Dict.Dict comparable a -> Dict.Dict comparable (List a) -> Dict.Dict comparable (List a)
mergeDicts s d =
    Dict.merge
        (\key a dict -> Dict.insert key [ a ] dict)
        (\key a b dict -> Dict.insert key (a :: b) dict)
        (\key b dict -> Dict.insert key b dict)
        s
        d
        Dict.empty
