module Table exposing (Row, listToRow, rowToList, stringToRow, testRow, rowToMatrix)


type Row
    = Nothing
    | Just String Row
    | Up String Row
    | Down String Row


testRow : Row
testRow =
    Just "x" (Just "0" (Up "len" (Just "1" (Down "xuong" (Just "0" Nothing)))))


rowToList : Int -> Row -> List ( String, Int )
rowToList start row =
    case row of
        Nothing ->
            []

        Just value tail ->
            ( value, start ) :: rowToList start tail

        Up value tail ->
            ( value, start + 1 ) :: rowToList (start + 2) tail

        Down value tail ->
            ( value, start - 1 ) :: rowToList (start - 2) tail


listToRow : List String -> Row
listToRow list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case head of
                "len" ->
                    Up "\\nearrow" (listToRow tail)

                "xuong" ->
                    Down "\\searrow" (listToRow tail)

                _ ->
                    Just head (listToRow tail)


stringToRow : String -> Row
stringToRow string =
    string |> String.words |> listToRow


rowToMatrix : Row -> List (List String)
rowToMatrix row =
    let
        positionList =
            rowToList 0 row

        levels =
            positionList
                |> List.map Tuple.second

        highestLevel =
            List.maximum levels |> Maybe.withDefault 0

        lowestLevel =
            List.minimum levels |> Maybe.withDefault 0

        itemsOnLevel : List (String, Int) -> Int -> List String
        itemsOnLevel items level =
            List.map (\(label, lvl) -> if lvl == level then label else "") items

    in
        List.range lowestLevel highestLevel
            |> List.map (itemsOnLevel positionList)
            |> List.reverse