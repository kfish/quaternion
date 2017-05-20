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
            , fuzz Fuzz.float "setI f q |> getI == f" <|
                \f -> (getI (setI 7 unit)) |> floatEqual 7
            ]
        , describe "Yaw-Pitch-Roll tests"
            [ test "(fromYawPitchRoll >> toYawPitchRoll) (0, 0, 0) yaw" <|
                \() ->
                    let (yaw, pitch, roll) =
                            (fromYawPitchRoll >> toYawPitchRoll) (0, 0, 0)
                    in floatEqual 0 yaw
            , test "(fromYawPitchRoll >> toYawPitchRoll) (0, 0, 0) pitch" <|
                \() ->
                    let (yaw, pitch, roll) =
                            (fromYawPitchRoll >> toYawPitchRoll) (0, 0, 0)
                    in floatEqual 0 pitch
            , test "(fromYawPitchRoll >> toYawPitchRoll) (0, 0, 0) roll" <|
                \() ->
                    let (yaw, pitch, roll) =
                            (fromYawPitchRoll >> toYawPitchRoll) (0, 0, 0)
                    in floatEqual 0 roll
            , test "(fromYawPitchRoll >> toYawPitchRoll) ((pi/4), (pi/4), (pi/4)) yaw" <|
                \() ->
                    let (yaw, pitch, roll) =
                            (fromYawPitchRoll >> toYawPitchRoll) ((pi/4), (pi/4), (pi/4))
                    in floatEqual (pi/4) yaw
            , test "(fromYawPitchRoll >> toYawPitchRoll) ((pi/4), (pi/4), (pi/4)) pitch" <|
                \() ->
                    let (yaw, pitch, roll) =
                            (fromYawPitchRoll >> toYawPitchRoll) ((pi/4), (pi/4), (pi/4))
                    in floatEqual (pi/4) pitch
            , test "(fromYawPitchRoll >> toYawPitchRoll) ((pi/4), (pi/4), (pi/4)) roll" <|
                \() ->
                    let (yaw, pitch, roll) =
                            (fromYawPitchRoll >> toYawPitchRoll) ((pi/4), (pi/4), (pi/4))
                    in floatEqual (pi/4) roll
            ]
        , describe "Identity tests"
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
            , fuzz Fuzz.scalarVector "(fromSV >> toSV) sv == sv" <|
                \( s, v ) ->
                    let
                        ( s_, v_ ) =
                            (fromSV >> toSV) ( s, v )
                    in
                        Expect.all_
                            [ floatEqual s s_
                            , vec3Equal v v_
                            ]
            , fuzz Fuzz.floatTuple3 "(fromYawPitchRoll >> toYawPitchRoll) yaw == yaw" <|
                \( yaw, pitch, roll ) ->
                    let
                        ( yaw_, pitch_, roll_ ) =
                            (fromYawPitchRoll >> toYawPitchRoll) ( yaw, pitch, roll )
                    in
                        angleEqual yaw yaw_
            , fuzz Fuzz.floatTuple3 "(fromYawPitchRoll >> toYawPitchRoll) pitch == pitch" <|
                \( yaw, pitch, roll ) ->
                    let
                        ( yaw_, pitch_, roll_ ) =
                            (fromYawPitchRoll >> toYawPitchRoll) ( yaw, pitch, roll )
                    in
                        angleEqual pitch pitch_
            , fuzz Fuzz.floatTuple3 "(fromYawPitchRoll >> toYawPitchRoll) roll == roll" <|
                \( yaw, pitch, roll ) ->
                    let
                        ( yaw_, pitch_, roll_ ) =
                            (fromYawPitchRoll >> toYawPitchRoll) ( yaw, pitch, roll )
                    in
                        angleEqual roll roll_
            , fuzz Fuzz.quaternion "(negate >> negate) q == q" <|
                \q -> (Qn.negate >> Qn.negate) q |> qnEqual q
            , fuzz Fuzz.vec3 "(fromVec3 >> toVec3) v == v" <|
                \v -> (fromVec3 >> toVec3) v |> vec3Equal v
            ]
        , describe "property tests"
            [ fuzz Fuzz.quaternion "(normalize >> length) q == 1" <|
                \q -> (normalize >> length) q |> floatEqual 1.0
            ]
        ]
