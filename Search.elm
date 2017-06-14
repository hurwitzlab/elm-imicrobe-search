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
    , selectedOptions : List ( String, String )
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
        [ style [ ( "width", "100%" ) ] ]
        [ h1 [] [ text "Search" ]
        , div [ style [ ( "text-align", "center" ) ] ]
            [ text "Select: "
            , select [ onInput AddOption ]
                (List.map mkOption <| "-- Select --" :: Dict.keys model.optionList)
            ]
        , div [] (List.map optionToFormControl model.selectedOptions)
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


optionToFormControl : ( String, String ) -> Html msg
optionToFormControl ( optionName, dataType ) =
    let
        title =
            [ th [] [ text optionName ] ]

        el =
            case dataType of
                "number" ->
                    [ td []
                        [ text "Min: "
                        , input [ type_ "text", placeholder "min", name ("min__" ++ optionName) ] []
                        ]
                    , td []
                        [ text "Max: "
                        , input [ type_ "text", placeholder "max", name ("max__" ++ optionName) ] []
                        ]
                    ]

                _ ->
                    [ td [] [ input [ type_ "text", placeholder dataType ] [] ]
                    , td [] []
                    ]

        buttons =
            [ td [] [ input [ type_ "submit" ] [] ]
            , td [] [ input [ type_ "button" ] [] ]
            ]
    in
    table [ style [ ( "width", "100%" ) ] ]
        [ tr [] (title ++ el ++ buttons) ]



{--
optionToFormControl : ( String, String ) -> Html msg
optionToFormControl ( optionName, dataType ) =
    let
        title =
            span [ style [ ( "font-weight", "bold" ) ] ] [ text optionName ]

        el =
            case dataType of
                "number" ->
                    [ text "Min: "
                    , input [ type_ "text", placeholder "min", name ("min__" ++ optionName) ]
                        []
                    , text "Max: "
                    , input
                        [ type_ "text", placeholder "max", name ("max__" ++ optionName) ]
                        []
                    ]

                _ ->
                    [ input [ type_ "text", placeholder dataType ] [] ]
    in
    div []
        [ span [] (List.concat [ title :: el ]) ]
        --}
