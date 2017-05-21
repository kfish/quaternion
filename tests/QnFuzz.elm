module QnFuzz
    exposing
        ( quaternion
        , vec3
        , floatTuple3
        , floatTuple4
        , floatRecord4
        , scalarVector
        , yawPitchRoll
        )

import Fuzz exposing (Fuzzer, float, floatRange)
import Fuzz exposing (map, andThen, constant)
import Math.Quaternion as Qn exposing (Quaternion)
import Math.Vector3 as V3 exposing (Vec3)


quaternion : Fuzzer Quaternion
quaternion =
    Fuzz.map4 Qn.quaternion float float float float


vec3 : Fuzzer Vec3
vec3 =
    Fuzz.map3 V3.vec3 float float float


floatTuple3 : Fuzzer ( Float, Float, Float )
floatTuple3 =
    Fuzz.map3 (,,) float float float


floatTuple4 : Fuzzer ( Float, Float, Float, Float )
floatTuple4 =
    Fuzz.map4 (,,,) float float float float


floatRecord4 : Fuzzer { s : Float, i : Float, j : Float, k : Float }
floatRecord4 =
    Fuzz.map4 (\s i j k -> { s = s, i = i, j = j, k = k }) float float float float


scalarVector : Fuzzer ( Float, Vec3 )
scalarVector =
    Fuzz.map2 (,) float vec3



-- Is the given (yaw,pitch,roll) nearly vertical,
-- ie. towards the north or south pole, where the
-- conversion functions have a singularity


notVertical : ( Float, Float, Float ) -> Bool
notVertical ( yaw, pitch, roll ) =
    let
        cYaw =
            cos (yaw / 2)

        sYaw =
            sin (yaw / 2)

        cRoll =
            cos (roll / 2)

        sRoll =
            sin (roll / 2)

        cPitch =
            cos (pitch / 2)

        sPitch =
            sin (pitch / 2)

        q2 =
            cYaw * cRoll * sPitch + sYaw * sRoll * cPitch

        q3 =
            sYaw * cRoll * cPitch - cYaw * sRoll * sPitch
    in
        (abs (q2 * q2 + q3 * q3) <= 0.499)



-- yaw, pitch, roll in range, but could be vertical


rawYawPitchRoll : Fuzzer ( Float, Float, Float )
rawYawPitchRoll =
    Fuzz.map3 (,,)
        (floatRange 0 (2 * pi))
        (floatRange (-pi / 2) (pi / 2))
        (floatRange 0 (2 * pi))


yawPitchRoll : Fuzzer ( Float, Float, Float )
yawPitchRoll =
    conditional
        { retries = 10
        , fallback = always ( 0, 0, 0 )
        , condition = notVertical
        }
        rawYawPitchRoll



-- Backported from elm-test 4.0.0, remove when bumping dependency


{-| Conditionally filter a fuzzer to remove occasional undesirable

  - input. Takes a limit for how many retries to attempt, and a
  - fallback
  - function to, if no acceptable input can be found, create one from
  - an
  - unacceptable one. Also takes a condition to determine if the
  - input is
  - acceptable or not, and finally the fuzzer itself.
  - A good number of max retires is ten. A large number of retries
  - might
  - blow the stack.

-}
conditional : { retries : Int, fallback : a -> a, condition : a -> Bool } -> Fuzzer a -> Fuzzer a
conditional { retries, fallback, condition } fuzzer =
    if retries <= 0 then
        map fallback fuzzer
    else
        fuzzer
            |> andThen
                (\val ->
                    if condition val then
                        constant val
                    else
                        conditional { retries = (retries - 1), fallback = fallback, condition = condition } fuzzer
                )
