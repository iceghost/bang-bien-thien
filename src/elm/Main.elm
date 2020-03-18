port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Events as Events
import Icon
import Table


port sendTable : String -> Cmd msg


port copyToClipboard : String -> Cmd msg


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
    , copied : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        sample =
            "x 1 . 2 . 3\ny' . + 0 - .\ny -vc lên 3 xuống -vc"

        output =
            sample |> outputPipe
    in
    ( Model sample output "" False, sendTable output )


type Msg
    = Input String
    | Submit
    | CopyToClipboard
    | GetExample String


type Example
    = Fraction
    | Quaratic


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            ( { model | input = text, output = text |> outputPipe }
            , sendTable (text |> outputPipe)
            )

        Submit ->
            ( model, sendTable model.output )

        CopyToClipboard ->
            ( { model | copied = True }, copyToClipboard model.output )

        GetExample name ->
            let
                input = 
                    case name of                            
                        "y = -1/x" ->
                            "x -vc . . 0 . . +vc\nf' . + . || . +\nf 0 lên +vc ||-4 -vc lên 0"

                        _ ->
                            "x 1 . 2 . 3\ny' . + 0 - .\ny -vc lên 3 xuống -vc"

                output = input |> outputPipe
            in 
                ( { model | input = input, output = output }, sendTable output )


outputPipe : String -> String
outputPipe input =
    input
        |> String.lines
        |> List.map
            (Table.parse
                >> handleError
                >> Table.rowToMatrix
            )
        |> format


handleError : Result Table.Problem Table.Row -> Table.Row
handleError res =
    case res of
        Ok row ->
            row

        Err (Table.StuckAt col row) ->
            Table.singleton <| "\\text{Kiểm tra lại hàng này ở ký tự thứ " ++ String.fromInt col ++ "...}"

        Err (Table.ExpectingBracket col _) ->
            Table.singleton <| "\\text{Kiểm tra lại mấy cái ngoặc hàng này...}"

        Err (Table.BadString prob) ->
            Table.singleton <| prob


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
    div [ class "flex flex-col justify-between border-t-4 border-blue-500" ]
        [ main_ [ class "w-full md:w-3/4 lg:w-4/6 mx-auto bg-white h-full px-8 py-10" ]
            [ h1
                [ class "text-center text-4xl text-blue-700 font-semibold"
                , class "uppercase tracking-widest"
                ]
                [ text "Vẽ bảng biến thiên \\( \\mathsf{\\LaTeX} \\)" ]
            , div [ class "flex flex-col md:flex-row items-stretch justify-between mt-10" ]
                [ div [ class "flex flex-col w-full h-48 md:w-1/4 md:h-auto" ]
                    [ p [ class "text-xl font-semibold text-blue-700" ]
                        [ text "Nhập bảng biến thiên"
                        ]
                    , span [ class "block text-sm font-normal -mt-1" ]
                        [ text "... hoặc dùng"
                        , select [ Events.onInput GetExample, class "border-b border-dashed border-blue-900" ]
                            [ option [] [ text "bảng mẫu" ]
                            , option [] [ text "y = -1/x" ]
                            ]
                        ]
                    , textarea
                        [ class "w-full h-full mx-auto p-2 bg-blue-100 mt-1"
                        , Events.onInput Input
                        , Attrs.value model.input
                        ]
                        []
                    ]
                , div [ class "flex flex-col w-full mt-5 md:mt-0 md:w-3/4 md:pl-2" ]
                    [ a
                        [ Attrs.href "#"
                        , Attrs.id "copy"
                        , Attrs.attribute "data-clipboard-target" "#output"
                        , Events.onClick CopyToClipboard
                        , Attrs.title
                            (if model.copied then
                                "Đã copy!"

                             else
                                "Copy?"
                            )
                        , class "flex items-center text-xl font-semibold text-blue-700"
                        , class "hover:underline"
                        ]
                        [ text "Mã LaTeX "
                        , Icon.clipboard "w-6 ml-1 fill-current"
                        ]
                    , span [ class "text-sm font-normal -mt-1" ] [ text "Click bên trên để copy" ]
                    , pre
                        [ class "w-full self-stretch overflow-x-auto p-2 bg-blue-100"
                        , Attrs.id "output"
                        ]
                        [ text model.output
                        ]
                    ]
                ]
            , div [ class "flex flex-col text-center mt-2" ]
                [ span [ class "text-xl font-semibold text-blue-700 text-left my-4" ] [ text "Kết quả" ]
                , div
                    [ Attrs.id "math"
                    , class "overflow-x-auto mx-2"
                    ]
                    []
                , a
                    [ class "underline text-sm"
                    , Attrs.id "download"
                    ]
                    []
                ]
            , div [ class "border-t-2 border-dashed border-blue-700 mt-10" ]
                [ h1 [ class "text-xl font-semibold text-blue-700 mt-5" ] [ text "Hướng dẫn" ]
                , p [] [ text "Khi mới vào trang, bạn sẽ thấy một bảng biến thiên mẫu được nhập sẵn. Dưới đây là các bước để nhập 1 bảng biến thiên như vậy." ]
                , img [ Attrs.src "https://i.imgur.com/t75O6hd.png", class "w-full md:w-1/3 mx-auto" ] []
                , p [] [ text "Tưởng tượng bảng biến thiên của bạn được chia thành các cột và các hàng như sau" ]
                , img [ Attrs.src "https://i.imgur.com/11H8Jhg.png", class "w-full md:w-1/3 mx-auto" ] []
                , p [] [ text "Những ô nào còn trống thì thêm vào đó một dấu chấm. Các ký hiệu như \"vô cùng\" thì thay bằng \"vc\", mũi tên lên hay xuống thì thay bằng \"lên\" hoặc \"xuống\"" ]
                , img [ Attrs.src "https://i.imgur.com/hrx0wCp.png", class "w-full md:w-1/3 mx-auto" ] []
                , p [] [ text "Viết mỗi hàng thành một dòng chữ nữa là xong. Nhớ thêm dấu cách giữa các cột nhé." ]
                , img [ Attrs.src "https://i.imgur.com/Q3ZNF5a.png", class "w-full md:w-1/3 mx-auto" ] []
                ]
            , div []
                [ h1 [ class "text-xl font-semibold text-blue-700 mt-5" ] [ text "Danh sách cú pháp" ]
                , table [ class "table-auto" ]
                    [ thead []
                        [ tr []
                            [ td [ class "border px-4 py-2 font-semibold" ] [ text "Ký hiệu" ]
                            , td [ class "border px-4 py-2 font-semibold" ] [ text "Giải thích" ]
                            ]
                        ]
                    , tbody []
                        [ tr []
                            [ td [ class "border px-4 py-2" ] [ text "." ]
                            , td [ class "border px-4 py-2" ] [ text "Ký hiệu trống, không hiển thị gì cả" ]
                            ]
                        , tr []
                            [ td [ class "border px-4 py-2" ] [ text "lên" ]
                            , td [ class "border px-4 py-2" ] [ text "Ký hiệu mũi lên tên \\( \\nearrow \\) (\\nearrow trong \\(\\LaTeX\\))" ]
                            ]
                        , tr []
                            [ td [ class "border px-4 py-2" ] [ text "xuống" ]
                            , td [ class "border px-4 py-2" ] [ text "Ký hiệu mũi lên xuống \\( \\searrow \\) (\\searrow trong \\(\\LaTeX\\))" ]
                            ]
                        , tr []
                            [ td [ class "border px-4 py-2" ] [ text "vc" ]
                            , td [ class "border px-4 py-2" ] [ text "Ký hiệu vô cùng \\( \\infty \\) (\\infty trong \\(\\LaTeX\\))" ]
                            ]
                        , tr []
                            [ td [ class "border px-4 py-2" ] [ text "( ... )" ]
                            , td [ class "border px-4 py-2" ] [ text "Sử dụng 2 dấu ngoặc tròn để nhóm các biểu thức chứa khoảng cách trong ô" ]
                            ]
                        , tr []
                            [ td [ class "border px-4 py-2" ] [ text "||số" ]
                            , td [ class "border px-4 py-2" ] [ text "Ký hiệu không xác định, thay \"số\" bằng một số cụ thể (||1, ||2, ||-1, ||-2 chẳng hạn) để nâng lên hoặc hạ xuống ô ở đằng sau dấu || (mặc định là 0). Thử đi rồi biết" ]
                            ]
                        ]
                    ]
                ]
            ]
        , footer
            [ class "bg-blue-500 text-white text-center"
            ]
            [ div [ class "flex items-center justify-center" ]
                [ span [] [ text "Made with ❤ by N.D.K • " ]
                , a [ Attrs.href "https://github.com/iceghosttth/bang-bien-thien" ]
                    [ Icon.github "w-5 h-5 pl-1 fill-current" ]
                ]
            ]
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
    "\\begin{array}{c|"
        ++ String.repeat (max - 1) "c"
        ++ "}\n"
        ++ (list |> List.map (List.map (String.join "&") >> String.join "\\\\\n") |> String.join "\\\\\n\\hline\n")
        ++ "\n\\end{array}"
