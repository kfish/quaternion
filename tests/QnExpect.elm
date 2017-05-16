module QnExpect
    exposing
        ( floatEqual
        , qnEqual
        , vec3Equal
        )

import Expect exposing (Expectation)
import Math.Quaternion as Qn exposing (Quaternion, getScalar, getI, getJ, getK)
import Math.Vector3 as V3 exposing (Vec3, getX, getY, getZ)


floatRelativeTolerance : Float -> Float -> Float -> Bool
floatRelativeTolerance tolerance a b =
    if (a == b) then
        True
    else
        abs ((a - b) / a) < tolerance


equateWith : (a -> a -> String) -> (a -> a -> Bool) -> a -> a -> Expectation
equateWith failString f a b =
    if f a b then
        Expect.pass
    else
        Expect.fail (failString a b)


renderResult : String -> a -> a -> String
renderResult reason a b =
    toString a ++ "\n╷\n│ " ++ reason ++ "\n╵\n" ++ toString b


floatEqual : Float -> Float -> Expectation
floatEqual =
    let
        tolerance =
            0.0000001
    in
        equateWith
            (renderResult ("floats not within tolerance " ++ toString tolerance))
            (floatRelativeTolerance tolerance)


qnComponentRelativeTolerance : Float -> Quaternion -> Quaternion -> Bool
qnComponentRelativeTolerance tolerance a b =
    floatRelativeTolerance tolerance (getScalar a) (getScalar b)
        && floatRelativeTolerance tolerance (getI a) (getI b)
        && floatRelativeTolerance tolerance (getJ a) (getJ b)
        && floatRelativeTolerance tolerance (getK a) (getK b)


qnEqual : Quaternion -> Quaternion -> Expectation
qnEqual =
    let
        tolerance =
            0.0000001
    in
        equateWith
            (renderResult ("Components not within tolerance " ++ toString tolerance))
            (qnComponentRelativeTolerance tolerance)


vec3ComponentRelativeTolerance : Float -> Vec3 -> Vec3 -> Bool
vec3ComponentRelativeTolerance tolerance a b =
    floatRelativeTolerance tolerance (V3.getX a) (V3.getX b)
        && floatRelativeTolerance tolerance (V3.getY a) (V3.getY b)
        && floatRelativeTolerance tolerance (V3.getZ a) (V3.getZ b)


vec3Equal : Vec3 -> Vec3 -> Expectation
vec3Equal =
    let
        tolerance =
            0.0000001
    in
        equateWith
            (renderResult ("Components not within tolerance " ++ toString tolerance))
            (vec3ComponentRelativeTolerance tolerance)
