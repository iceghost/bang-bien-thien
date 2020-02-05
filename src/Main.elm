port module Main exposing (main)

import Browser
import Element as E
import Element.Background as Background
import Element.Border as B
import Element.Font as F
import Element.Input as I
import Html as H
import Html.Attributes as Attrs


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
    , output : String
    , status : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        sample =
            "x 1 . 2 . 3\ny' . + 0 - .\ny -vc lên 3 xuống -vc"
    in
    ( Model sample "" "", Cmd.none )


type Msg
    = Input String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            ( { model | input = text }, Cmd.none )

        Submit ->
            let
                output =
                    model.input |> logic |> viewTable |> format
            in
            ( { model | output = output }, sendTable output )


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


blue : E.Color
blue =
    E.rgb255 18 147 216


view : Model -> H.Html Msg
view model =
    E.layout [ F.family [ F.typeface "Open Sans" ] ] <|
        E.column [ E.width E.fill, E.height E.fill ]
            [ E.el [ E.height E.fill, E.width E.fill ] <|
                E.column
                    [ E.centerY
                    , E.centerX
                    , 400 |> E.px |> E.width
                    , E.spacing 10
                    ]
                    [ E.el
                        [ E.centerX
                        , F.extraBold
                        , F.family [ F.typeface "Roboto" ]
                        , F.variant F.smallCaps
                        , F.color blue
                        ]
                        (E.text "Công cụ dịch bảng biến thiên sang \\( \\LaTeX \\)")
                    , E.column [ E.spacingXY 0 5, F.hairline ]
                        [ I.multiline [ 400 |> E.px |> E.width, 200 |> E.px |> E.height ]
                            { spellcheck = False
                            , onChange = Input
                            , text = model.input
                            , placeholder = Nothing
                            , label = I.labelHidden "Math input"
                            }
                        , I.button
                            [ E.alignRight
                            , B.width 1
                            , E.padding 5
                            , B.color (E.rgb255 186 189 182)
                            , B.rounded 3
                            ]
                            { onPress = Just Submit
                            , label = E.text "Dịch..."
                            }
                        ]
                    , E.el [ E.centerX, F.italic ] (E.text model.status)
                    , E.el [ E.htmlAttribute <| Attrs.id "math", E.centerX, F.size 20 ] <| E.none
                    , E.link
                        [ E.htmlAttribute <| Attrs.id "download"
                        , E.centerX
                        , F.underline
                        , F.size 13
                        ]
                        { url = "", label = E.text "" }
                    ]
            , E.el
                [ E.width E.fill
                , F.size 15
                , E.paddingXY 0 10
                , Background.color blue
                , F.color (E.rgb 1 1 1)
                ]
              <|
                E.el [ E.centerX ] <|
                    E.text "Made by Khang with ❤"
            ]


viewRow : List Position -> List (List String)
viewRow list =
    let
        ( labels, levels ) =
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
