module QnFuzz
    exposing
        ( quaternion
        , vec3
        , floatTuple4
        , floatRecord4
        , scalarVector
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


floatTuple4 : Fuzzer ( Float, Float, Float, Float )
floatTuple4 =
    Fuzz.map4 (,,,) float float float float


floatRecord4 : Fuzzer { s : Float, i : Float, j : Float, k : Float }
floatRecord4 =
    Fuzz.map4 (\s i j k -> { s = s, i = i, j = j, k = k }) float float float float


scalarVector : Fuzzer ( Float, Vec3 )
scalarVector =
    Fuzz.map2 (,) float vec3
