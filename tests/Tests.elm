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
            ]
        , describe "Getter/setter tests"
            [ fuzz Fuzz.float "(fromscalar >> getScalar) == identity" <|
                \f -> (fromScalar >> getScalar) f |> floatEqual f
            ]
        , describe "Identity tests"
            [ fuzz Fuzz.quaternion "(negate >> negate) == identity" <|
                \q -> (Qn.negate >> Qn.negate) q |> qnEqual q
            , fuzz Fuzz.vec3 "(fromVec3 >> toVec3) == identity" <|
                \v -> (fromVec3 >> toVec3) v |> vec3Equal v
            ]
        ]
