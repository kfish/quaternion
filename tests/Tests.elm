module Tests exposing (all)

import Test exposing (..)
import Expect
import Fuzz
import String
import Math.Quaternion as Qn exposing (..)
import QnExpect as Expect exposing (..)
import QnFuzz as Fuzz exposing (..)


all : Test
all =
    describe "Quaternion Test Suite"
        [ describe "Trivial tests"
            [ test "Unit" <|
                \() ->
                    qnEqual unit (fromScalar 1)
            , test "i=7" <|
                \() ->
                    floatEqual 7.0 (getI (setI 7 unit))
            ]
        , describe "Getter/setter tests"
            [ fuzz Fuzz.float "(fromScalar >> getScalar) q == q" <|
                \f -> (fromScalar >> getScalar) f |> floatEqual f
            , fuzz Fuzz.float "(fromscalar >> getScalar) q == q" <|
                \f -> (fromScalar >> getScalar) f |> floatEqual f
            , fuzz Fuzz.float "setI f q |> getI == f"  <|
                \f -> (getI (setI 7 unit)) |> floatEqual 7
            ]
        , describe "Identity tests"
            [ fuzz Fuzz.quaternion "(negate >> negate) q == q" <|
                \q -> (Qn.negate >> Qn.negate) q |> qnEqual q
            , fuzz Fuzz.vec3 "(fromVec3 >> toVec3) v == v" <|
                \v -> (fromVec3 >> toVec3) v |> vec3Equal v
            ]
        ]
