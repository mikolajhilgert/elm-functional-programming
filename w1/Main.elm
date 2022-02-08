module Main exposing (main)

import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Char exposing (toLower)
import List exposing (length)
import Char exposing (fromCode, toCode, isUpper, isLower)



--
-- Encode & Decode
--

char_bounds: Int-> Char -> Char -> Char -> Char
char_bounds offset char upper lower = 
    if toCode char + offset > toCode upper then
        fromCode (((toCode char + offset) - toCode upper) + (toCode lower - 1))
    else if  toCode char + offset < toCode upper then
        fromCode (((toCode char + offset) - toCode upper) + (toCode lower + 1))
    else fromCode (toCode char + offset)

encode: Int -> Char -> Char
encode offset char =
    if isUpper char then
        char_bounds offset char 'Z' 'A'
    else if isLower char then
        char_bounds offset char 'z' 'a'
    else char

decode: Int -> Char -> Char
decode offset char =
    if isUpper char then
        char_bounds (0 - offset) char 'A' 'Z'
    else if isLower char then
        char_bounds (0 - offset) char 'a' 'z'
    else char

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


main : Html Never
main =
    Html.div
        [ Html.Attributes.style "padding" "20px" ]
        [ ExerciseRunner.fontStyles
        , caesar1
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
        ]
