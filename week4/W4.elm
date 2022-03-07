module W4 exposing (..)

-- steps:
--      install Elm: https://guide.elm-lang.org/install/elm.html
--      start a terminal
--      run "elm init"
--      copy this file to the new ./src directory
--      rename this file to your own name, and rename the module (see top line above) accordingly


import Html exposing (Html)
import Char exposing (toLower)
import List exposing (length)
import String exposing (fromInt,fromFloat)
import Basics exposing (modBy,abs)
import Tuple exposing (first, second)


-- your functions:

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

graph: Function -> Float -> Float -> Float -> Float -> String
graph func xmin xmax ymin ymax =
    if xmax == xmin then
        ""
    else
        (drawLine (eval xmax func) ymin ymax ymin) ++ String.fromFloat(eval xmax func) ++ "\n" ++ graph func xmin (xmax - 1) ymin ymax

drawLine: Float -> Float -> Float -> Float -> String
drawLine value lower upper current =
    if current < value && current <= upper then
        "*" ++ (drawLine value lower upper (current+1))
    else if current <= upper then
        "-" ++ (drawLine value lower upper (current+1))
    else 
        ""


-- collecting results for printing:

-- arbitrary list:

my_results: List String
my_results =
    [
        --(Minus (Poly (Minus (Div (X) (Const 5)) (Const 1)) 4) (Plus (Poly (Plus (Div (X) (Const -2)) (Const 2)) (Const 2)) (Const 6)))
        "-- Hello-Elm output --\n\n  cube calculations:",
        (graph (Minus (Poly (Minus (Div (X) (Const 5)) (Const 1)) 4) (Plus (Poly (Plus (Div (X) (Const -2)) (Const 2)) 2) (Const 6))) -10 20 -10 10),
        "\n-- end --" 
    ] 
    
-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 80

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)
    