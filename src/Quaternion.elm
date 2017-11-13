module Quaternion exposing (..)

{-| Quaternions

This API exposes only normalized quaternions (ie. length=1).
To use arbitrary quaternions, import Internal.Quaternion.


# Types

@docs Quaternion


# Create

@docs unit, fromVec3, fromTo


# Angle-Axis representation

@docs fromAngleAxis, getAngle, getAxis


# Operations

@docs negate, conjugate, multiply, rotate


# Yaw-Pitch-Roll

@docs fromYawPitchRoll, toYawPitchRoll


# Conversions

@docs toMat4

-}

import Quaternion.Internal as Internal
import Math.Matrix4 as M4
import Math.Vector3 as V3 exposing (Vec3, vec3)


{-| Quaternion type
-}
type alias Quaternion =
    Internal.Quaternion


{-| Construction of a unit quaternion
-}
unit : Quaternion
unit =
    Internal.unit


{-| Construct a quaternion from an orientation vector
-}
fromVec3 : Vec3 -> Quaternion
fromVec3 v =
    Internal.normalize (Internal.fromVec3 v)


{-| Quaternion from two vectors
-}
fromTo : Vec3 -> Vec3 -> Quaternion
fromTo =
    Internal.fromTo


{-| Quaternion negation: -a
-}
negate : Quaternion -> Quaternion
negate =
    Internal.negate


{-| Quaternion conjugate
-}
conjugate : Quaternion -> Quaternion
conjugate =
    Internal.conjugate


{-| Hamilton product
-}
multiply : Quaternion -> Quaternion -> Quaternion
multiply =
    Internal.hamilton


{-| Rotate a vector v by the unit quaternion q
-}
rotate : Quaternion -> Vec3 -> Vec3
rotate =
    Internal.rotateV


{-| Construction from angle, axis.
This will create a unit quaternion if given a unit vector.
-}
fromAngleAxis : Float -> Vec3 -> Quaternion
fromAngleAxis =
    Internal.fromAngleAxis


{-| Angle of rotation.
Returns angle in radians, in the range [0, 2*pi)
-}
getAngle : Quaternion -> Float
getAngle =
    Internal.getAngle


{-| Unit vector along the axis of rotation
-}
getAxis : Quaternion -> Vec3
getAxis =
    Internal.getAxis


{-| Construction a unit quaternion from Tait-Bryan angles representing
(yaw, pitch, roll)

<https://en.wikipedia.org/wiki/Euler_angles#Tait.E2.80.93Bryan_angles>

-}
fromYawPitchRoll : ( Float, Float, Float ) -> Quaternion
fromYawPitchRoll =
    Internal.fromYawPitchRoll


{-| Convert a unit quaternion to Yaw, Pitch, Roll
-}
toYawPitchRoll : Quaternion -> ( Float, Float, Float )
toYawPitchRoll =
    Internal.toYawPitchRoll


{-| Convert to a Mat4
-}
toMat4 : Quaternion -> M4.Mat4
toMat4 =
    Internal.toMat4


{-| Spherical linear interpolation
 -}
slerp : Float -> Quaternion -> Quaterion -> Quaternion
slerp =
    Internal.slerp
