module Main exposing (main)

import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Char exposing (toLower)
import List exposing (length)
import String exposing (fromInt,fromFloat)
import Basics exposing (modBy,abs)
import Tuple exposing (first, second)

--
-- Modelling math functions
--

type Function
    = Poly Function Float
    | Mult Function Function
    | Div Function Function
    | Plus Function Function
    | Minus Function Function
    | Const Float
    | X

print: Function -> String
print func = 
    case func of
        Poly base power -> "("++(print base)++"^"++fromFloat power++")"
        Mult left right -> "("++(print left) ++ "*" ++ (print right)++")"
        Div top bottom -> "("++(print top)++")" ++ "/" ++ "("++(print bottom)++")"
        Plus left right -> "("++(print left) ++ "+" ++ (print right)++")"
        Minus left right-> "("++(print left) ++ "-" ++ (print right)++")"
        Const value -> fromFloat value 
        X -> "x"

eval: Float -> Function -> Float
eval x func =
    case func of
        Poly base power -> (eval x base) ^ power
        Mult left right -> (eval x left) * (eval x right)
        Div top bottom ->  (eval x top) / (eval x bottom)
        Plus left right -> (eval x left) + (eval x right)
        Minus left right-> (eval x left) - (eval x right)
        Const value -> value 
        X -> x

graph: Function -> Int -> Int -> Int -> Int -> String
graph: func xmin xmax ymin ymax =
    if xmax == xmin then
        ""
    else
        (drawLine (eval xmax func) ymin ymax ymin) ++ graph func xmin (xmax - 1) ymin ymax

drawLine: Float -> Int -> Int -> String
drawLine value lower upper current =
    if current < value and current <= upper then
        "*" ++ (drawLine value lower upper (current+1))
    else if current <= upper then
        "-" ++ (drawLine value lower upper (current+1))
    else 
        ""


-- REPRESENTATION
math1 : List ( String, List ExerciseRunner.Example )
math1 =
    [ ( "Modelling math functions (part 1)"
        , [ ExerciseRunner.functionExample1 "print"
            print
            [ ( Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2), "(((3+x)*(x-(x^5)))+2)" )]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample2 "eval"
            eval
            [ ((2.0,(Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2))), -148.0 )]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample2 "eval"
            eval
            [ ((2.0,(Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2))), "" )]
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
