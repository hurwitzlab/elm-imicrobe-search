module Main exposing (..)

import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import RemoteData exposing (..)


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
    , selectedOptions : List ( String, String )
    , optionValue : Dict.Dict String String
    , searchResults : WebData (Dict.Dict (List String))
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



--    | UpdateSearchResults WebData (Dict.dict (List String)))


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

        RemoveOption opt ->
            ( { model | selectedOptions = rmOption model.selectedOptions opt }, Cmd.none )

        UpdateOptionValue opt val ->
            ( { model | optionValue = Dict.insert opt val model.optionValue }, Cmd.none )

        Search ->
            -- ( model, doSearch )
            ( model, Cmd.none )



{--
        UpdateSearchResults (Ok newResults) ->
            ( { model | searchResults = newResults }, Cmd.none )
            --}
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
        foo =
            Debug.log "getting options" 1

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

        Success a ->
            text "All good!"



{--
doSearch options =
    let
        url =
            "https://www.imicrobe.us/sample/search_results.json"

        payload =
            Encode.object 0 model.optionValue
    in
    Http.post url decoder
        |> RemoteData.sendRequest
        |> Cmd.map UpdateSearchResults
        --}
