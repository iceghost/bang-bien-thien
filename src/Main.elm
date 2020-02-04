port module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as Attrs
import Html.Events as Events
import Update.Extra as Update


port sendTable : String -> Cmd msg


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
            ( { model | status = "Submitted" :: model.status }, sendTable (model.input |> logic |> viewTable |> format) )
                |> Update.andThen update Check

        Check ->
            ( { model | status = "Valid" :: model.status }, Cmd.none )


type alias Position =
    ( String, Int )


logic : String -> List (List Position)
logic text =
    let
        list =
            String.lines text
    in
    List.map
        (String.words
            >> List.foldl reduce ( 0, [] )
            >> Tuple.second
            >> List.reverse
        )
        list


reduce : String -> ( Int, List Position ) -> ( Int, List Position )
reduce text ( pos, list ) =
    case text of
        "lên" ->
            ( pos + 2, ( "\\nearrow", pos + 1 ) :: list )

        "xuống" ->
            ( pos - 2, ( "\\searrow", pos - 1 ) :: list )

        "." ->
            ( pos, ( "", pos ) :: list )
        
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
        , H.div [ Attrs.id "math" ] [ H.text "\\( wait \\)" ]
        ]


viewRow : List Position -> List (List String)
viewRow list =
    let
        ( labels, levels ) =
            list |> List.unzip

        max =
            Maybe.withDefault 0 (List.maximum levels)

        min =
            Maybe.withDefault 0 (List.minimum levels)
    in
    List.map
        (\level ->
            List.map
                (\( label, pos ) ->
                    if pos == level then
                        label

                    else
                        ""
                )
                list
        )
        (List.range min max)
        |> List.reverse


viewTable : List (List Position) -> List (List (List String))
viewTable list =
    List.map viewRow list


format : List (List (List String)) -> String
format list =
    let
        max =
            List.map (List.head >> Maybe.withDefault [] >> List.length) list |> List.maximum |> Maybe.withDefault 0
    in
    "\\( \\begin{array}{c|"
        ++ String.repeat (max - 1) "c"
        ++ "}"
        ++ (list |> List.map (List.map (String.join "&") >> String.join "\\\\") |> String.join "\\\\ \\hline ")
        ++ "\\end{array} \\)"
