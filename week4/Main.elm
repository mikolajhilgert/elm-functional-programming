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
        Poly -> 
        Mult -> 
        Div -> 
        Plus -> 
        Minus -> 
        Const -> 
        X -> 


-- REPRESENTATION
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
        , [ ExerciseRunner.functionExample3 "isTriple"
            isTriple
            [ ( (3, 4, 5), True )
            , ( (3, 4, 6), False )
            ]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample1 "pythTriple "
            pythTriple
            [ ( (5,4), (9,40,41) )]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample1 "isTripleTuple"
            isTripleTuple
            [ ( (9,40,41), True )]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample1 "isTripleTuple with pythTriple "
            isTripleTuple
            [( (pythTriple (5,4)) , True )]
            ]
        )
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
