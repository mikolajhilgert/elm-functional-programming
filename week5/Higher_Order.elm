module Higher_Order exposing (..)

import Html exposing (Html)
import Basics exposing (toFloat)

-- your functions:

aboveVal: Int -> Int -> Bool
aboveVal val x = 
    x > val

double: Int -> Int
double x =
    x * 2

repeatUntil: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil predicate operation input =
    if ((predicate input) == False) then
        repeatUntil predicate operation (operation input)
    else
        input

repeatUntilCollatz: (a -> Bool) -> (a -> a) -> List a -> List a
repeatUntilCollatz predicate operation input =
    case input of
        [] -> []
        (x :: xs) ->
            if ((predicate x) == False) then
                repeatUntilCollatz predicate operation ((operation x) :: input)
            else
                input

myCollatz: Int -> Int
myCollatz x =
    if (Basics.modBy 2 x == 0) then
        x//2
    else
        1 + (3*x)


-- collecting results for printing:

-- arbitrary list:
my_results: List String
my_results =
    [
        "repeatUntil (aboveVal 100) double 7 = " ++ pr (repeatUntil (aboveVal 100) double 7)++"\n\n",
        "repeatUntil (aboveVal 100) ((+) 1) 42 = "++ pr (repeatUntil (aboveVal 100) ((+) 1) 42)++"\n\n",
        "repeatUntilCollatz ((==)1) myCollatz [19] = "++ pr (repeatUntilCollatz ((==)1) myCollatz [19])++"\n\n"
    ] 
    
-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 250

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
    
