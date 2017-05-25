module InternalTests exposing (all)

import Test exposing (..)
import Expect
import Fuzz
import String
import Math.Matrix4 as M4
import Math.Vector3 as V3
import Internal.Quaternion as Qn exposing (..)
import QnExpect as Expect exposing (..)
import QnFuzz as Fuzz exposing (..)


all : Test
all =
    describe "Internal.Quaternion Test Suite"
        [ testTrivial
        , testGetterSetter
        , testIdentity
        , testOperators
        , testMultiplication
        , testCayleyGraph
        ]


testTrivial : Test
testTrivial =
    describe "Trivial tests"
        [ test "Unit" <|
            \() ->
                qnEqual unit (fromScalar 1)
        ]


testGetterSetter : Test
testGetterSetter =
    describe "Getter/setter tests"
        [ fuzz Fuzz.float "(fromScalar >> getScalar) q == q" <|
            \f -> (fromScalar >> getScalar) f |> floatEqual f
        , fuzz2 Fuzz.float Fuzz.quaternion "setScalar f q |> getScalar == f" <|
            \f q -> (getScalar (setScalar f q)) |> floatEqual f
        , fuzz2 Fuzz.float Fuzz.quaternion "setI f q |> getI == f" <|
            \f q -> (getI (setI f q)) |> floatEqual f
        , fuzz2 Fuzz.float Fuzz.quaternion "setJ f q |> getI == f" <|
            \f q -> (getJ (setJ f unit)) |> floatEqual f
        , fuzz2 Fuzz.float Fuzz.quaternion "setK f q |> getI == f" <|
            \f q -> (getK (setK f unit)) |> floatEqual f
        ]


testIdentity : Test
testIdentity =
    describe "Identity tests"
        [ fuzz Fuzz.floatTuple4 "(fromTuple >> toTuple) t == t" <|
            \( s, i, j, k ) ->
                let
                    ( s_, i_, j_, k_ ) =
                        (fromTuple >> toTuple) ( s, i, j, k )
                in
                    Expect.all_
                        [ floatEqual s s_
                        , floatEqual i i_
                        , floatEqual j j_
                        , floatEqual k k_
                        ]
        , fuzz Fuzz.floatRecord4 "(fromRecord >> toRecord) r == r" <|
            \input ->
                let
                    output =
                        (fromRecord >> toRecord) input
                in
                    Expect.all_
                        [ floatEqual input.s output.s
                        , floatEqual input.i output.i
                        , floatEqual input.j output.j
                        , floatEqual input.k output.k
                        ]
        , fuzz Fuzz.scalarVector "(fromScalarVector >> toScalarVector) sv == sv" <|
            \( s, v ) ->
                let
                    ( s_, v_ ) =
                        (fromScalarVector >> toScalarVector) ( s, v )
                in
                    Expect.all_
                        [ floatEqual s s_
                        , vec3Equal v v_
                        ]
        , fuzz Fuzz.vec3 "(fromVec3 >> toVec3) v == v" <|
            \v -> (fromVec3 >> toVec3) v |> vec3Equal v
        ]


testOperators : Test
testOperators =
    describe "Operator tests"
        [ fuzz Fuzz.quaternion "(negate >> negate) q == q" <|
            \q -> (Qn.negate >> Qn.negate) q |> qnEqual q
        , fuzz Fuzz.nonZeroQuaternion "(normalize >> length) q == 1" <|
            \q -> (normalize >> length) q |> floatEqual 1.0
        , fuzz Fuzz.quaternion "lengthSquared q == (length q * length q)" <|
            \q -> length q * length q |> floatEqual (lengthSquared q)
        ]


testMultiplication : Test
testMultiplication =
    describe "Multiplication tests"
        [ fuzz2 Fuzz.float Fuzz.quaternion "Multiplication by a scalar on the right" <|
            \f q -> Qn.hamilton q (Qn.fromScalar f) |> qnEqual (Qn.scale f q)
        , fuzz2 Fuzz.float Fuzz.quaternion "Multiplication by a scalar on the left" <|
            \f q -> Qn.hamilton (Qn.fromScalar f) q |> qnEqual (Qn.scale f q)
        ]


{-| Test the multiplication of basis vectors

  - <https://en.wikipedia.org/wiki/Quaternion#Algebraic_properties>

-}
testCayleyGraph : Test
testCayleyGraph =
    let
        i =
            Qn.quaternion 0 1 0 0

        j =
            Qn.quaternion 0 0 1 0

        k =
            Qn.quaternion 0 0 0 1
    in
        describe "Cayley graph of Q(8)"
            [ describe "Multiplication on the right by i"
                [ test "1 * i == i" <|
                    \() -> Qn.hamilton unit i |> qnEqual i
                , test "i * i == -1" <|
                    \() -> Qn.hamilton i i |> qnEqual (Qn.negate Qn.unit)
                , test "-1 * i == -i" <|
                    \() -> Qn.hamilton (Qn.negate Qn.unit) i |> qnEqual (Qn.negate i)
                , test "-i * i == 1" <|
                    \() -> Qn.hamilton (Qn.negate i) i |> qnEqual unit
                , test "k * i == j" <|
                    \() -> Qn.hamilton k i |> qnEqual j
                , test "j * i == -k" <|
                    \() -> Qn.hamilton j i |> qnEqual (Qn.negate k)
                , test "-k * i == -j" <|
                    \() -> Qn.hamilton (Qn.negate k) i |> qnEqual (Qn.negate j)
                , test "-j * i == k" <|
                    \() -> Qn.hamilton (Qn.negate j) i |> qnEqual k
                ]
            , describe "Multiplication on the right by j"
                [ test "1 * j == j" <|
                    \() -> Qn.hamilton unit j |> qnEqual j
                , test "j * j == -1" <|
                    \() -> Qn.hamilton j j |> qnEqual (Qn.negate Qn.unit)
                , test "-1 * j == -j" <|
                    \() -> Qn.hamilton (Qn.negate Qn.unit) j |> qnEqual (Qn.negate j)
                , test "-j * j == 1" <|
                    \() -> Qn.hamilton (Qn.negate j) j |> qnEqual unit
                , test "k * j == -i" <|
                    \() -> Qn.hamilton k j |> qnEqual (Qn.negate i)
                , test "-i * j == -k" <|
                    \() -> Qn.hamilton (Qn.negate i) j |> qnEqual (Qn.negate k)
                , test "-k * j == i" <|
                    \() -> Qn.hamilton (Qn.negate k) j |> qnEqual i
                , test "i * j == k" <|
                    \() -> Qn.hamilton i j |> qnEqual k
                ]
            , describe "Multiplication on the right by k"
                [ test "1 * k == k" <|
                    \() -> Qn.hamilton unit k |> qnEqual k
                , test "k * k == -1" <|
                    \() -> Qn.hamilton k k |> qnEqual (Qn.negate Qn.unit)
                , test "-1 * k == -k" <|
                    \() -> Qn.hamilton (Qn.negate Qn.unit) k |> qnEqual (Qn.negate k)
                , test "-k * k == 1" <|
                    \() -> Qn.hamilton (Qn.negate k) k |> qnEqual unit
                , test "j * k == i" <|
                    \() -> Qn.hamilton j k |> qnEqual i
                , test "i * k == -j" <|
                    \() -> Qn.hamilton i k |> qnEqual (Qn.negate j)
                , test "-j * k == -i" <|
                    \() -> Qn.hamilton (Qn.negate j) k |> qnEqual (Qn.negate i)
                , test "-i * k == j" <|
                    \() -> Qn.hamilton (Qn.negate i) k |> qnEqual j
                ]
            ]
