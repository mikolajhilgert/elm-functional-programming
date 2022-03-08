module Main exposing (main)

import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Char exposing (toLower)
import List exposing (drop, length)
import String exposing (fromFloat, fromInt)
import Basics exposing (modBy,abs)
import Tuple exposing (first, second)

--
-- Modelling math functions
--

-- Merge sort
split: Int->List Int -> (List Int, List Int)
split i list =
    case list of
        [] -> ([],[])
        (x::xs) ->
            if ((modBy 2 i) == 0) then
                if length list >= i then
                   (x::first (split i xs),drop (i - 1) xs)
                else
                    ([], [])
            else
                if length list > i then
                   (x::first (split i xs),drop (i - 1) xs)
                else
                    ([], [])

merge: List Int -> List Int -> List Int
merge left right =
    case (left,right) of
        ([],[]) -> []
        (_,[]) -> left
        ([],_) -> right
        (x::xs,y::ys) ->
            if x < y then
                x :: merge xs right
            else
                y :: merge left ys

msort: List Int -> List Int
msort input =
    if length input >= 1 then
        merge (msort(first (split (round(toFloat(length input) / 2 )) input))) (msort(second (split (round(toFloat(length input) / 2 )) input)))
    else
        msort(first (split (round(toFloat(length input) / 2 )) input))

--msort: List Int -> List Int
--msort input =
--    case input of
--        [] -> []
--        _ -> merge (msort(first (split (round(toFloat(length input) / 2 )) input))) (msort(second (split (round(toFloat(length input) / 2 )) input)))
--

testCase a =
    (first (split (round(toFloat(length a) / 2 )) a))


-- REPRESENTATION
math1 : List ( String, List ExerciseRunner.Example )
math1 =
    [
    -- , ( "HTML", [] )
    ( "MergeSort"
            , [ ExerciseRunner.functionExample2 "Split"
                split
                [(((round(toFloat(length [9,5,4,18,3,2]) / 2 )), [9,5,4,18,3,2]),  ([2,3,4,5],[7,8,9,10]))]
            ]
    ),
     ( ""
         , [ ExerciseRunner.functionExample1 "Merge"
             testCase
             [ ([9,5,4,18,3,2,1],  [2,3,5,6,8,9])]
         ]
     )
    ]


main : Html Never
main =
    Html.div
        [ Html.Attributes.style "padding" "20px" ]
        [ ExerciseRunner.fontStyles
        , math1
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
        ]
