module Lindenmayer exposing (LSys, startWith, addRule, execute, showRules)

import Dict exposing (Dict)


type alias LSys =
    { start : List Char
    , rules : Dict Char (List Char)
    }


showRules : LSys -> List String
showRules lsys =
    ("Start: " ++ String.fromList lsys.start)
        :: (lsys.rules
                |> Dict.toList
                |> List.map (\( c, cs ) -> String.fromChar c ++ " -> " ++ String.fromList cs)
           )


startWith : String -> LSys
startWith s =
    LSys (String.toList s) Dict.empty


addRule : Char -> String -> LSys -> LSys
addRule c s lsys =
    let
        rules2 =
            Dict.insert c (String.toList s) lsys.rules
    in
        { lsys | rules = rules2 }


apply : LSys -> Char -> List Char
apply lsys c =
    case Dict.get c lsys.rules of
        Just cs ->
            cs

        Nothing ->
            [ c ]


expand : LSys -> Int -> List Char -> List Char
expand lsys depth chars =
    if depth <= 0 then
        chars
    else
        chars
            |> List.concatMap (apply lsys)
            |> expand lsys (depth - 1)


execute : (Char -> Maybe c) -> Int -> LSys -> List c
execute interpret depth lsys =
    expand lsys depth lsys.start
        |> List.filterMap interpret
