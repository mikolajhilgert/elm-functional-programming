module Main exposing (main)

import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Char exposing (toLower)
import List exposing (length)
import Char exposing (fromCode, toCode, isUpper, isLower)
import Basics exposing (modBy)

--
-- Caesar
--
shift_char: Int -> Char -> Char -> Char
shift_char offset char bound = 
    fromCode (modBy 26 (toCode char - toCode bound + offset) + toCode bound)

encode: Int -> Char -> Char
encode offset char =
    if isLower char then
        shift_char offset char 'a'
    else if isUpper char then
        shift_char offset char 'A'
    else
        char

decode: Int -> Char -> Char
decode offset char =
    encode (0 - offset) char



--
-- Pythagoras
--

sqr: Int -> Int
sqr val = val * val

isTriple: Int -> Int -> Int -> Bool
isTriple adj op hyp = 
    adj > 0 && op > 0 && hyp > 0 && ((sqr adj + sqr op) == sqr hyp)




caesar1 : List ( String, List ExerciseRunner.Example )
caesar1 =
    [ ( "Caesar (part 1)"
        , [ ExerciseRunner.functionExample2 "encode"
            encode
            [ ( (5, 'x'), 'c' )
            , ( (7, 'T'), 'A' )
            ]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample2 "decode"
            decode
            [ ( (5, 'c'), 'x' )
            , ( (7, 'A'), 'T' )
            ]
        ]
        )
    -- , ( "HTML", [] )
    ]

pythagoras1 : List ( String, List ExerciseRunner.Example )
pythagoras1 =
    [ ( "Pythagoras (part 1)"
        , [ ExerciseRunner.functionExample3 "sqr & adj"
            isTriple
            [ ( (3, 4, 5), True )
            , ( (3, 4, 6), False )
            ]
        ]
        )
    -- , ( "HTML", [] )
    ]


main : Html Never
main =
    Html.div
        [ Html.Attributes.style "padding" "20px" ]
        [ ExerciseRunner.fontStyles
        , caesar1
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
                    , pythagoras1
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
        ]
