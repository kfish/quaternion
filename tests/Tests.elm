module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, float, tuple, string)
import String

import Math.Quaternion as Qn exposing (..)

floatRelativeTolerance : Float -> Float -> Float -> Bool
floatRelativeTolerance tolerance a b =
    if (a == b) then True
    else abs ((a-b)/a) < tolerance

equateWith : (a -> a -> String) -> (a -> a -> Bool) -> a -> a -> Expect.Expectation
equateWith failString f a b =
    if f a b then Expect.pass
    else Expect.fail (failString a b)

renderResult : String -> a -> a -> String
renderResult reason a b = toString a ++ "\n╷\n│ " ++ reason ++ "\n╵\n" ++ toString b

floatEqual : Float -> Float -> Expect.Expectation
floatEqual =
    let tolerance = 0.0000001
    in equateWith
        (renderResult ("floats not within tolerance " ++ toString tolerance))
        (floatRelativeTolerance tolerance)

qnComponentRelativeTolerance : Float -> Quaternion -> Quaternion -> Bool
qnComponentRelativeTolerance tolerance a b =
    floatRelativeTolerance tolerance (getScalar a) (getScalar b)
    && floatRelativeTolerance tolerance (getI a) (getI b)
    && floatRelativeTolerance tolerance (getJ a) (getJ b)
    && floatRelativeTolerance tolerance (getK a) (getK b)

qnEqual : Quaternion -> Quaternion -> Expect.Expectation
qnEqual =
    let tolerance = 0.0000001
    in equateWith
        (renderResult ("Components not within tolerance " ++ toString tolerance))
        (qnComponentRelativeTolerance tolerance)

quaternion : Fuzzer Quaternion
quaternion = Fuzz.map4 Qn.quaternion float float float float

all : Test
all =
    describe "Quaternion Test Suite"
        [ describe "Trivial tests"
            [ test "Unit" <|
                \() ->
                    qnEqual unit (fromScalar 1)
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            ]
        , describe "Getter/setter tests"
            [ fuzz float "(fromscalar >> getScalar) == identity" <|
                \f -> (fromScalar >> getScalar) f |> floatEqual f
            , fuzz quaternion "(negate >> negate) == identity" <|
                \q -> (Qn.negate >> Qn.negate) q |> qnEqual q
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
