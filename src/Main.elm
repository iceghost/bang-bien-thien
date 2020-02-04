module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as Attrs
import Html.Events as Events
import Update.Extra as Update


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { input : String
    , status : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [], Cmd.none )


type Msg
    = Input String
    | Submit
    | Check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            ( { model | input = text }, Cmd.none )

        Submit ->
            ( { model | status = "Submitted" :: model.status }, Cmd.none )
                |> Update.andThen update Check

        Check ->
            ( { model | status = "Valid" :: model.status }, Cmd.none )


type alias Position =
    ( String, Int )


logic : String -> List Position
logic text =
    let
        list =
            String.words text
    in
    list
        |> (( 0, [] )
                |> List.foldl
                    reduce
           )
        |> Tuple.second
        |> List.reverse


reduce : String -> ( Int, List Position ) -> ( Int, List Position )
reduce text ( pos, list ) =
    case text of
        "u" ->
            ( pos + 2, ( text, pos + 1 ) :: list )

        "d" ->
            ( pos - 2, ( text, pos - 1 ) :: list )

        _ ->
            ( pos, ( text, pos ) :: list )


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.div []
            [ H.textarea [ Events.onInput Input ] [ H.text model.input ]
            , H.button [ Events.onClick Submit ] [ H.text "Dịch..." ]
            ]
        , H.ul [] <|
            List.map (\status -> H.li [] [ H.text status ]) model.status
        , H.p []
            [ H.text (Debug.toString (logic model.input |> List.unzip))
            ]
        , H.pre []
            (List.map (H.p [] << List.singleton << H.text) (model.input |> logic |> viewTable))
        ]


viewTable : List Position -> List String
viewTable list =
    let
        ( labels, levels ) =
            list |> List.unzip

        max =
            Maybe.withDefault 0 (List.maximum levels)

        min =
            Maybe.withDefault 0 (List.minimum levels)
    in
    List.map 
        (\level -> List.foldl (\(label, pos) result -> if pos == level then result ++ label else result ++ " ") "" list) 
        (List.range min max)
            |> List.reverse
