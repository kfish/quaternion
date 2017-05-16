module QnFuzz
    exposing
        ( quaternion
        , vec3
        )

import Fuzz exposing (Fuzzer, list, int, float, tuple, string)
import Math.Quaternion as Qn exposing (Quaternion)
import Math.Vector3 as V3 exposing (Vec3)


quaternion : Fuzzer Quaternion
quaternion =
    Fuzz.map4 Qn.quaternion float float float float


vec3 : Fuzzer Vec3
vec3 =
    Fuzz.map3 V3.vec3 float float float
