port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Events as Events


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
    , status : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        sample =
            "x 1 . 2 . 3\ny' . + 0 - .\ny -vc lên 3 xuống -vc"
    in
    ( Model sample "", sendTable (sample |> outputPipe) )


type Msg
    = Input String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            update Submit { model | input = text }

        Submit ->
            ( model, sendTable (model.input |> outputPipe) )


outputPipe : String -> String
outputPipe input =
    input |> logic |> viewTable |> format


type alias Position =
    ( String, Maybe Int )


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
            ( pos + 2, ( "\\nearrow", Just (pos + 1) ) :: list )

        "xuống" ->
            ( pos - 2, ( "\\searrow", Just (pos - 1) ) :: list )

        "." ->
            ( pos, ( "", Just pos ) :: list )

        "+vc" ->
            ( pos, ( "+\\infty", Just pos ) :: list )

        "-vc" ->
            ( pos, ( "-\\infty", Just pos ) :: list )

        "||" ->
            ( pos, ( "||", Nothing ) :: list )

        _ ->
            ( pos, ( text, Just pos ) :: list )


view : Model -> Html Msg
view model =
    main_ [ class "flex flex-col justify-between h-screen border-t-4 border-blue-500" ]
        [ div [ class "container mx-auto" ]
            [ h1
                [ class "text-center text-3xl font-black text-blue-500 uppercase"
                , class "mt-10"
                ]
                [ text "Vẽ bảng biến thiên \\( \\mathbf{\\LaTeX} \\)" ]
            , textarea
                [ class "block w-full md:w-2/3 lg:w-1/2 mx-auto h-56 border p-3 mt-10"
                , Events.onInput Input
                , Attrs.value model.input
                ]
                []
            , button
                [ class "block mx-auto border p-1 mt-2 rounded"
                , class "hover:bg-blue-400 hover:text-white"
                , Events.onClick Submit
                ]
                [ text "Xử lý" ]
            , div [ class "block text-center mt-2" ]
                [ div
                    [ Attrs.id "math"
                    , class "overflow-x-auto mx-2"
                    ]
                    []
                , a
                    [ class "underline"
                    , Attrs.id "download"
                    ]
                    []
                ]
            ]
        , footer
            [ class "bg-blue-500 text-white text-center"
            , class "mt-10 md:mt-0"
            ]
            [ p [] [ text "Made with ❤ by N.D.K \u{2022} "
            , a [ Attrs.href "https://github.com/iceghosttth/bang-bien-thien" ] [ i [ class "fab fa-github"] [] ] ] ]
        ]


viewRow : List Position -> List (List String)
viewRow list =
    let
        ( _, levels ) =
            list |> List.unzip

        max =
            Maybe.withDefault 0 (levels |> List.map (Maybe.withDefault 0) |> List.maximum)

        min =
            Maybe.withDefault 0 (levels |> List.map (Maybe.withDefault 0) |> List.minimum)
    in
    List.map
        (\level ->
            List.map
                (\( label, pos ) ->
                    if pos == Just level || pos == Nothing then
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
