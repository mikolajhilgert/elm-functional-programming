module Main exposing (main)

import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Char exposing (toLower)
import List exposing (length)
import Char exposing (fromCode, toCode, isUpper, isLower)
import Basics exposing (modBy)
import Tuple exposing (first, second)

--
-- Graph
--

type Function
    = Poly Function Int
    | Mult Function Function
    | Div Function Function
    | Plus Function Function
    | Minus Function Function
    | Const Int
    | X

print: Function -> String
print func = 
    case func of
        Poly -> "^"
        Mult -> "*"
        Div -> "/"
        Plus -> "+"
        Minus -> "-"
        Const -> ""
        X -> ""


-- REPRESENTATION
math1 : List ( String, List ExerciseRunner.Example )
math1 =
    [ ( "Modelling math functions (part 1)"
        , [ ExerciseRunner.functionExample1 "encode"
            print
            [ ( Poly, "^" )]
        ]
        )
    -- , ( "HTML", [] )
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
