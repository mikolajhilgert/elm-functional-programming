module Main exposing (main)

import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Char exposing (toLower)
import List exposing (filter, length, map)
import Char exposing (fromCode, toCode, isUpper, isLower) 
import Pythagoras exposing (isTripleTuple, pythTriple)
import String exposing (fromChar, fromList, toList)
import Caesar exposing (encodeChar, decodeChar, encode, sanitize, normalize, decode)

--
-- Caesar (Part 3)
--

containsCanary: String -> String -> Bool
containsCanary canary text =
    case (toList canary, toList text) of
    (x :: xs, y :: ys) ->
        if x == y then
            containsCanary (fromList xs) (fromList ys)
        else 
            containsCanary canary (fromList ys)
    ( [], _ ) -> True
    ( _, [] ) -> False


repeat: Int -> List String -> String -> List(Int,String)
repeat n can text =
    case can of
    (x :: xs) ->
        if n == 26 then
            repeat 0 xs text
        else 
            if containsCanary x (decode n text) then
                (n,(decode n text)) :: repeat (n+1) can text
            else
                repeat (n+1) can text
    [] ->
        []

candidates: List String -> String -> List (Int, String)
candidates canaries text = 
    (repeat 0 canaries text)

-- REPRESENTATION
caesar3 : List ( String, List ExerciseRunner.Example )
caesar3 =
    [ ( "Caesar (part 3)"
        , [ ExerciseRunner.functionExample2 "candidates"
            candidates
            [ ( (["AND","THE"],"DGGADBCOOCZYMJHZYVMTOJOCZHVS"),  [(5,"YBBVYWXJJXUTHECUTQHOJEJXUCQN"),(14,"PSSMPNOAAOLKYVTLKHYFAVAOLTHE"),(21,"ILLFIGHTTHEDROMEDARYTOTHEMAX")] )]
        ]
    )
    -- , ( "HTML", [] )
    ]

main : Html Never
main =
    Html.div
        [ Html.Attributes.style "padding" "20px" ]
        [ ExerciseRunner.fontStyles
        , caesar3
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
        ]
