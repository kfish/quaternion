module Tests exposing (all)

import Test exposing (..)
import Expect
import Fuzz
import String
import Math.Matrix4 as M4
import Math.Vector3 as V3
import Quaternion as Qn exposing (..)
import QnExpect as Expect exposing (..)
import QnFuzz as Fuzz exposing (..)
import InternalTests


all : Test
all =
    let
        testFromTo : Test
        testFromTo =
            describe "Construction from two vectors"
                [ fuzz2 Fuzz.nonZeroVec3 Fuzz.nonZeroVec3 "rotate (fromTo u v) u == v" <|
                    \u v -> Qn.rotate (fromTo u v) u |> vec3Collinear v
                , fuzz Fuzz.nonZeroVec3 "fromTo v v == unit" <|
                    \v -> fromTo v v |> qnEqual unit
                ]

        testAngleAxis : Test
        testAngleAxis =
            describe "Angle-Axis representation"
                [ fuzz2 Fuzz.angle Fuzz.nonZeroVec3 "(fromAngleAxis >> getAngle)" <|
                    \angle axis -> fromAngleAxis angle axis |> getAngle |> floatEqual angle
                , fuzz2 Fuzz.angle Fuzz.unitVec3 "(fromAngleAxis >> getAxis)" <|
                    \angle axis -> fromAngleAxis angle axis |> getAxis |> vec3Equal axis
                , fuzz Fuzz.unitQuaternion "(toAngleAxis >> fromAngleAxis) q == q" <|
                    \q -> fromAngleAxis (getAngle q) (getAxis q) |> qnEqual q
                ]

        testRotation : Test
        testRotation =
            describe "Rotation tests"
                [ fuzz3 Fuzz.float Fuzz.nonZeroVec3 Fuzz.vec3 "Vector rotation via Angle Axis" <|
                    \angle axis v ->
                        Qn.rotate (Qn.fromAngleAxis angle axis) v
                            |> vec3Equal (M4.transform (M4.makeRotate angle axis) v)
                ]

        testYawPitchRoll : Test
        testYawPitchRoll =
            describe "Yaw-Pitch-Roll tests"
                [ test "(fromYawPitchRoll >> toYawPitchRoll) (0, 0, 0) yaw" <|
                    \() ->
                        let
                            ( yaw, pitch, roll ) =
                                (fromYawPitchRoll >> toYawPitchRoll) ( 0, 0, 0 )
                        in
                            floatEqual 0 yaw
                , test "(fromYawPitchRoll >> toYawPitchRoll) (0, 0, 0) pitch" <|
                    \() ->
                        let
                            ( yaw, pitch, roll ) =
                                (fromYawPitchRoll >> toYawPitchRoll) ( 0, 0, 0 )
                        in
                            floatEqual 0 pitch
                , test "(fromYawPitchRoll >> toYawPitchRoll) (0, 0, 0) roll" <|
                    \() ->
                        let
                            ( yaw, pitch, roll ) =
                                (fromYawPitchRoll >> toYawPitchRoll) ( 0, 0, 0 )
                        in
                            floatEqual 0 roll
                , test "(fromYawPitchRoll >> toYawPitchRoll) ((pi/4), (pi/4), (pi/4)) yaw" <|
                    \() ->
                        let
                            ( yaw, pitch, roll ) =
                                (fromYawPitchRoll >> toYawPitchRoll) ( (pi / 4), (pi / 4), (pi / 4) )
                        in
                            floatEqual (pi / 4) yaw
                , test "(fromYawPitchRoll >> toYawPitchRoll) ((pi/4), (pi/4), (pi/4)) pitch" <|
                    \() ->
                        let
                            ( yaw, pitch, roll ) =
                                (fromYawPitchRoll >> toYawPitchRoll) ( (pi / 4), (pi / 4), (pi / 4) )
                        in
                            floatEqual (pi / 4) pitch
                , test "(fromYawPitchRoll >> toYawPitchRoll) ((pi/4), (pi/4), (pi/4)) roll" <|
                    \() ->
                        let
                            ( yaw, pitch, roll ) =
                                (fromYawPitchRoll >> toYawPitchRoll) ( (pi / 4), (pi / 4), (pi / 4) )
                        in
                            floatEqual (pi / 4) roll
                , fuzz Fuzz.yawPitchRoll "(fromYawPitchRoll >> toYawPitchRoll) yaw " <|
                    \( yaw, pitch, roll ) ->
                        let
                            ( yaw_, pitch_, roll_ ) =
                                (fromYawPitchRoll >> toYawPitchRoll) ( yaw, pitch, roll )
                        in
                            angleEqual yaw yaw_
                , fuzz Fuzz.yawPitchRoll "(fromYawPitchRoll >> toYawPitchRoll) pitch " <|
                    \( yaw, pitch, roll ) ->
                        let
                            ( yaw_, pitch_, roll_ ) =
                                (fromYawPitchRoll >> toYawPitchRoll) ( yaw, pitch, roll )
                        in
                            angleEqual pitch pitch_
                , fuzz Fuzz.yawPitchRoll "(fromYawPitchRoll >> toYawPitchRoll) roll " <|
                    \( yaw, pitch, roll ) ->
                        let
                            ( yaw_, pitch_, roll_ ) =
                                (fromYawPitchRoll >> toYawPitchRoll) ( yaw, pitch, roll )
                        in
                            angleEqual roll roll_
                ]

        testAngleAxisYawPitchRoll : Test
        testAngleAxisYawPitchRoll =
            describe "Angle Axis representation"
                [ fuzz Fuzz.float "Yaw is rotation about the z axis" <|
                    \f -> Qn.fromAngleAxis f V3.k |> qnEqual (Qn.fromYawPitchRoll ( f, 0, 0 ))
                , fuzz Fuzz.float "Pitch is rotation about the y axis" <|
                    \f -> Qn.fromAngleAxis f V3.j |> qnEqual (Qn.fromYawPitchRoll ( 0, f, 0 ))
                , fuzz Fuzz.float "Roll is rotation about the x axis" <|
                    \f -> Qn.fromAngleAxis f V3.i |> qnEqual (Qn.fromYawPitchRoll ( 0, 0, f ))
                ]

        testMatrix4Conversion : Test
        testMatrix4Conversion =
            describe "Conversion to Matrix4"
                [ fuzz2 Fuzz.rotationQuaternion Fuzz.nonZeroVec3 "(toMat4 >> transform) == vrotate" <|
                    \q v -> M4.transform (Qn.toMat4 q) v |> vec3Equal (Qn.rotate q v)
                ]
    in
        describe "Quaternion Test Suite" <|
            [ InternalTests.all
            , testFromTo
            , testAngleAxis
            , testRotation

            , testYawPitchRoll
            , testAngleAxisYawPitchRoll
            -- , testMatrix4Conversion
            ]
