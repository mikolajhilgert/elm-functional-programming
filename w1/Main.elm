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


encode: Int -> Char -> Char
encode offset char =
    if isUpper char then
        if toCode char + offset > toCode 'Z' then
            fromCode (((toCode char + offset) - toCode 'Z') + (toCode 'A' - 1))
        else fromCode (toCode char + offset)
    else if isLower char then
        if toCode char + offset > toCode 'z' then
            fromCode (((toCode char + offset) - toCode 'z') + (toCode 'a' - 1))
        else fromCode (toCode char + offset)
    else char

decode: Int -> Char -> Char
decode offset char =
    if isUpper char then
        if toCode char - offset < toCode 'A' then
            fromCode (((toCode char - offset) - toCode 'A') + (toCode 'Z' + 1))
        else fromCode (toCode char - offset)
    else if isLower char then
        if toCode char - offset < toCode 'a' then
            fromCode (((toCode char - offset) - toCode 'a') + (toCode 'z' + 1))
        else fromCode (toCode char - offset)
    else char

examples : List ( String, List ExerciseRunner.Example )
examples =
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
        , examples
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
        ]
