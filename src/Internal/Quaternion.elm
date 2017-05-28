module Internal.Quaternion exposing (..)

{-| Quaternions

Can express rotations, but not shearing, scaling or translations

Translations are trivial

Shearing and scaling don't happen in the real world

Scaling is trivial

So, a combination of translation and orientation (via quaternions)
is everything you need for world transforms.

Impossible transforms are inexpressible

Also, quaternions are faster than matrices due to 4 components not 9.


# API Design

This API is designed for efficiency: input ranges are documented, but
input values are not sanitized. For example, most rotation operations
require unit quaternions and unit axis vectors. Rather than normalizing
all inputs, which would add sqrt calls to all functions, the expected
values are documented.


# Types

@docs Quaternion


# Create

@docs unit, quaternion, fromScalar, fromVec3, fromScalarVector, fromTo


# Get and Set

@docs getScalar, getI, getJ, getK, setScalar, setI, setJ, setK


# Operations

@docs length, lengthSquared, normalize, negate, scale, add, sub, conjugate, hamilton, vmult, rotateV, multv, rotateQ


# Angle-Axis representation

@docs fromAngleAxis, getAngle, getAxis


# Yaw-Pitch-Roll

@docs fromYawPitchRoll, toYawPitchRoll


# Conversions

@docs toScalarVector, toVec3, toMat4


# Co-ordinate systems

Rigid Body Dynamics, Inertial Reference Frames,
and Graphics Coordinate Systems:
A Resolution of Conflicting Conventions and Terminology
<http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.58.99&rep=rep1&type=pdf>

-}

import Math.Matrix4 as M4
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (Vec4, vec4)


{-| Quaternion type
-}
type alias Quaternion =
    Vec4


{-| Creates a new 4-element quaternion with the given x, y, z, and w values.
-}
quaternion : Float -> Float -> Float -> Float -> Quaternion
quaternion =
    V4.vec4


{-| Construction of a unit quaternion
-}
unit : Quaternion
unit =
    vec4 0 0 0 1


{-| Construction of a scalar quaternion
-}
fromScalar : Float -> Quaternion
fromScalar s =
    vec4 0 0 0 s


{-| Construction of a right quaternion
-}
fromVec3 : Vec3 -> Quaternion
fromVec3 v =
    let
        { x, y, z } =
            V3.toRecord v
    in
        vec4 x y z 0


{-| Construct a Quaternion from its representation as a scalar and a vector
-}
fromScalarVector : ( Float, Vec3 ) -> Quaternion
fromScalarVector ( s, v ) =
    let
        { x, y, z } =
            V3.toRecord v
    in
        quaternion x y z s


{-| Extract the scalar component of a quaternion.
-}
getScalar : Quaternion -> Float
getScalar =
    V4.getW


{-| Extract the i component of a quaternion.
-}
getI : Quaternion -> Float
getI =
    V4.getX


{-| Extract the j component of a quaternion.
-}
getJ : Quaternion -> Float
getJ =
    V4.getY


{-| Extract the k component of a quaternion.
-}
getK : Quaternion -> Float
getK =
    V4.getZ


{-| Update the scalar component of a quaternion, returning a new quaternion.
-}
setScalar : Float -> Quaternion -> Quaternion
setScalar =
    V4.setW


{-| Update the i component of a quaternion, returning a new quaternion.
-}
setI : Float -> Quaternion -> Quaternion
setI =
    V4.setX


{-| Update the j component of a quaternion, returning a new quaternion.
-}
setJ : Float -> Quaternion -> Quaternion
setJ =
    V4.setY


{-| Update the k component of a quaternion, returning a new quaternion.
-}
setK : Float -> Quaternion -> Quaternion
setK =
    V4.setZ


{-| Convert a quaternion to a tuple of (scalar, vector)
-}
toScalarVector : Quaternion -> ( Float, Vec3 )
toScalarVector q =
    let
        { x, y, z, w } =
            V4.toRecord q
    in
        ( w, vec3 x y z )


{-| Extract the axis of rotation
-}
toVec3 : Quaternion -> Vec3
toVec3 q =
    let
        { x, y, z, w } =
            V4.toRecord q
    in
        vec3 x y z


{-| The length of the given quaternion: |a|
-}
length : Quaternion -> Float
length =
    V4.length


{-| The square of the length of the given quaternion: |a| * |a|
-}
lengthSquared : Quaternion -> Float
lengthSquared =
    V4.lengthSquared


{-| A unit quaternion with the same direction as the given quaternion: a / |a|
-}
normalize : Quaternion -> Quaternion
normalize =
    V4.normalize


{-| Quaternion negation: -a
-}
negate : Quaternion -> Quaternion
negate =
    V4.negate


{-| Multiply the quaternion by a scalar: s * q
-}
scale : Float -> Quaternion -> Quaternion
scale =
    V4.scale


{-| Quaternion addition: a + b
-}
add : Quaternion -> Quaternion -> Quaternion
add =
    V4.add


{-| Quaternion subtraction: a - b
-}
sub : Quaternion -> Quaternion -> Quaternion
sub =
    V4.sub


{-| Quaternion conjugate
-}
conjugate : Quaternion -> Quaternion
conjugate q =
    let
        { x, y, z, w } =
            V4.toRecord q
    in
        V4.fromTuple ( -x, -y, -z, w )


{-| Hamilton product
-}
hamilton : Quaternion -> Quaternion -> Quaternion
hamilton q1 q2 =
    let
        ( b1, c1, d1, a1 ) =
            V4.toTuple q1

        ( b2, c2, d2, a2 ) =
            V4.toTuple q2
    in
        quaternion
            (a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2)
            (a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2)
            (a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2)
            (a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2)



{-
   let (s1, v1) = toScalarVector q1
       (s2, v2) = toScalarVector q2
       s = s1*s2 - V3.dot v1 v2
       v3_add = V3.add
       v = V3.scale s1 v2 `v3_add` V3.scale s2 v1 `v3_add` V3.cross v1 v2
   in fromScalarVector (s,v)
-}


{-| Multiplication of a quaternion by a vector
-}
multv : Quaternion -> Vec3 -> Quaternion
multv q v =
    hamilton q (fromVec3 v)


{-| Multiplication of a vector by a quaternion
-}
vmult : Vec3 -> Quaternion -> Quaternion
vmult v q =
    hamilton (fromVec3 v) q


{-| Rotate quaternion q1 by quaternion q2
-}
rotateQ : Quaternion -> Quaternion -> Quaternion
rotateQ q1 q2 =
    hamilton q1 (hamilton q2 (conjugate q1))


{-| Rotate a vector v by the unit quaternion q
-}
rotateV : Quaternion -> Vec3 -> Vec3
rotateV q =
    fromVec3 >> rotateQ q >> toVec3


{-| Quaternion from two vectors
-}



-- http://lolengine.net/blog/2014/02/24/quaternion-from-two-vectors-final


fromTo : Vec3 -> Vec3 -> Quaternion
fromTo u v =
    let
        norm_u_norm_v =
            sqrt (V3.dot u u * V3.dot v v)

        real_part0 =
            norm_u_norm_v + V3.dot u v

        ( real_part, w ) =
            if real_part0 < 1.0e-6 * norm_u_norm_v then
                let
                    w =
                        if abs (V3.getX u) > abs (V3.getZ u) then
                            vec3 -(V3.getY u) (V3.getX u) 0
                        else
                            vec3 0 -(V3.getZ u) (V3.getY u)
                in
                    ( 0, w )
            else
                ( real_part0, V3.cross u v )
    in
        normalize <| fromScalarVector ( real_part, w )


{-| Construction from angle, axis.
This will create a unit quaternion if given a unit vector.
-}
fromAngleAxis : Float -> Vec3 -> Quaternion
fromAngleAxis angle axis =
    let
        { x, y, z } =
            V3.toRecord (V3.normalize axis)

        theta =
            angle / 2.0

        c =
            cos theta

        s =
            sin theta
    in
        quaternion (x * s) (y * s) (z * s) c


{-| Angle of rotation.
Returns angle in radians, in the range [0, 2*pi)
-}
getAngle : Quaternion -> Float
getAngle q =
    2.0 * acos (getScalar q)


{-| Unit vector along the axis of rotation
-}
getAxis : Quaternion -> Vec3
getAxis q =
    V3.normalize (toVec3 q)


{-| Construction a unit quaternion from Tait-Bryan angles representing
(yaw, pitch, roll)

<https://en.wikipedia.org/wiki/Euler_angles#Tait.E2.80.93Bryan_angles>

-}
fromYawPitchRoll : ( Float, Float, Float ) -> Quaternion
fromYawPitchRoll ( yaw, pitch, roll ) =
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

        q0 =
            cYaw * cRoll * cPitch + sYaw * sRoll * sPitch

        q1 =
            cYaw * sRoll * cPitch - sYaw * cRoll * sPitch

        q2 =
            cYaw * cRoll * sPitch + sYaw * sRoll * cPitch

        q3 =
            sYaw * cRoll * cPitch - cYaw * sRoll * sPitch
    in
        quaternion q1 q2 q3 q0


{-| Convert a unit quaternion to Yaw, Pitch, Roll
<http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToEuler/index.htm>
-}
toYawPitchRoll : Quaternion -> ( Float, Float, Float )
toYawPitchRoll q =
    let
        ( q1, q2, q3, q0 ) =
            V4.toTuple q

        q2q2 =
            q2 * q2

        t =
            q2q2 + q3 * q3

        ( yaw, pitch, roll ) =
            if (t > 0.499) then
                -- Singularity at north pole
                ( 2 * atan2 q1 q0, pi / 2, 0 )
            else if (t < -0.499) then
                -- Singularity at south pole
                ( -2 * atan2 q1 q0, -pi / 2, 0 )
            else
                let
                    r0 =
                        2 * (q0 * q1 + q2 * q3)

                    r1 =
                        1 - 2 * (q1 * q1 + q2q2)

                    roll_ =
                        atan2 r0 r1

                    t2_0 =
                        2 * (q0 * q2 - q3 * q1)

                    t2_1 =
                        if t2_0 > 1.0 then
                            1.0
                        else
                            t2_0

                    t2 =
                        if t2_1 < -1.0 then
                            -1.0
                        else
                            t2_1

                    pitch_ =
                        asin t2

                    t3 =
                        2.0 * (q0 * q3 + q1 * q2)

                    t4 =
                        1 - 2.0 * t

                    yaw_ =
                        atan2 t3 t4
                in
                    ( yaw_, pitch_, roll_ )
    in
        ( yaw, pitch, roll )


{-| Convert to a Mat4
-}
toMat4 : Quaternion -> M4.Mat4
toMat4 q =
    let
        ( yaw, pitch, roll ) =
            toYawPitchRoll q
    in
        M4.makeRotate yaw V3.k
            |> M4.rotate pitch V3.j
            |> M4.rotate roll V3.i
