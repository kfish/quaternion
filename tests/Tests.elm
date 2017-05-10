module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, float, tuple, string)
import String

import Math.Quaternion exposing (..)

floatEqual : Float -> Float -> Expect.Expectation
floatEqual a b =
    if (a == b) then Expect.equal a b
    else Expect.lessThan 0.0000001 <| abs ((a-b)/a)

all : Test
all =
    describe "Quaternion Test Suite"
        [ describe "Trivial tests"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            ]
        , describe "Getter/setter tests"
            [ fuzz float "get (fromscalar)" <|
                \f -> getScalar (fromScalar f) |> floatEqual f
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]
