module Math.Quaternion exposing (..)

{-|

Quaternions

Can express rotations, but not shearing, scaling or translations

Translations are trivial

Shearing and scaling don't happen in the real world

Scaling is trivial

So, a combination of translation and orientation (via quaternions)
is everything you need for world transforms.

Impossible transforms are inexpressible

Also, quaternions are faster than matrices due to 4 components not 9.

# Types
@docs Quaternion

# Create
@docs unit, quaternion, fromAngleAxis, fromRecord, fromSV, fromScalar, fromTo, fromTo2, fromTuple, fromVec3, orient, fromYawPitchRoll, toYawPitchRoll

# Get and Set
@docs getI, getJ, getK, getScalar, getAngle, getAxis, setI, setJ, setK, setScalar

# Operations
@docs length, lengthSquared, normalize, negate, scale, add, sub, conjugate, hamilton, vmult, vrotate, multv, rotate


# Conversions
@docs toMat4, toRecord, toSV, toTuple, toVec3

-}

import Math.Matrix4 as M4
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (Vec4, vec4)

{-| Quaternion type -}
type alias Quaternion = Vec4

{-| Creates a new 4-element quaternion with the given x, y, z, and w values. -}
quaternion : Float -> Float -> Float -> Float -> Quaternion
quaternion = V4.vec4

{-| Construction of a unit quaternion -}
unit : Quaternion
unit = vec4 1 0 0 0

{-| Construction of a scalar quaternion -}
fromScalar : Float -> Quaternion
fromScalar s = vec4 s 0 0 0

{-| Construction of a right quaternion -}
fromVec3 : Vec3 -> Quaternion
fromVec3 v =
    let {x,y,z} = V3.toRecord v
    in vec4 0 x y z

{-| Extract the scalar component of a quaternion. -}
getScalar : Quaternion -> Float
getScalar = V4.getX

{-| Extract the i component of a quaternion. -}
getI : Quaternion -> Float
getI = V4.getY

{-| Extract the j component of a quaternion. -}
getJ : Quaternion -> Float
getJ = V4.getZ

{-| Extract the k component of a quaternion. -}
getK : Quaternion -> Float
getK = V4.getW

{-| Update the scalar component of a quaternion, returning a new quaternion. -}
setScalar : Float -> Quaternion -> Quaternion
setScalar = V4.setX

{-| Update the i component of a quaternion, returning a new quaternion. -}
setI : Float -> Quaternion -> Quaternion
setI = V4.setY

{-| Update the j component of a quaternion, returning a new quaternion. -}
setJ : Float -> Quaternion -> Quaternion
setJ = V4.setZ

{-| Update the k component of a quaternion, returning a new quaternion. -}
setK : Float -> Quaternion -> Quaternion
setK = V4.setW

{-| Convert a quaternion to a tuple. -}
toTuple : Quaternion -> (Float,Float,Float,Float)
toTuple = V4.toTuple

{-| Convert a tuple to a quaternion. -}
fromTuple : (Float,Float,Float,Float) -> Quaternion
fromTuple = V4.fromTuple

{-| Convert a quaternion to a record. -}
toRecord : Quaternion -> { s:Float, i:Float, j:Float, k:Float }
toRecord q =
    let {x,y,z,w} = V4.toRecord q
    in { s=x, i=y, j=z, k=w }

{-| Convert a record to a quaternion. -}
fromRecord : { s:Float, i:Float, j:Float, k:Float } -> Quaternion
fromRecord {s,i,j,k} = quaternion s i j k

{-| Convert a quaternion to a tuple of (scalar, vector) -}
toSV : Quaternion -> (Float, Vec3)
toSV q =
    let {x,y,z,w} = V4.toRecord q
    in (x, vec3 y z w)

{-| Construct a Quaternion from its representation as a scalar and a vector -}
fromSV : (Float, Vec3) -> Quaternion
fromSV (s,v) =
    let {x,y,z} = V3.toRecord v
    in quaternion s x y z

{-| Quaternion addition: a + b -}
add : Quaternion -> Quaternion -> Quaternion
add = V4.add

{-| Quaternion subtraction: a - b -}
sub : Quaternion -> Quaternion -> Quaternion
sub = V4.sub

{-| Quaternion negation: -a -}
negate : Quaternion -> Quaternion
negate = V4.negate

{-| The length of the given quaternion: |a| -}
length : Quaternion -> Float
length = V4.length

{-| The square of the length of the given quaternion: |a| * |a| -}
lengthSquared : Quaternion -> Float
lengthSquared = V4.lengthSquared

{-| A unit quaternion with the same direction as the given quaternion: a / |a| -}
normalize : Quaternion -> Quaternion
normalize = V4.normalize

{-| Multiply the quaternion by a scalar: s * q -}
scale : Float -> Quaternion -> Quaternion
scale = V4.scale

{-| Extract the axis of rotation -}
toVec3 : Quaternion -> Vec3
toVec3 q = let {x,y,z,w} = V4.toRecord q in vec3 y z w

{-| Quaternion conjugate -}
conjugate : Quaternion -> Quaternion
conjugate q =
    let {x,y,z,w} = V4.toRecord q
    in fromRecord { s = x , i = -y, j = -z, k = -w }

{-| Hamilton product -}
hamilton : Quaternion -> Quaternion -> Quaternion
hamilton q1 q2 =
    let (a1, b1, c1, d1) = toTuple q1
        (a2, b2, c2, d2) = toTuple q2
    in quaternion (a1*a2 - b1*b2 - c1*c2 - d1*d2)
                  (a1*b2 + b1*a2 + c1*d2 - d1*c2)
                  (a1*c2 - b1*d2 + c1*a2 + d1*b2)
                  (a1*d2 + b1*c2 - c1*b2 + d1*a2)
{-
    let (s1, v1) = toSV q1
        (s2, v2) = toSV q2
        s = s1*s2 - V3.dot v1 v2
        v3_add = V3.add
        v = V3.scale s1 v2 `v3_add` V3.scale s2 v1 `v3_add` V3.cross v1 v2
    in fromSV (s,v)
-}

{-| Multiplication of a quaternion by a vector -}
multv : Quaternion -> Vec3 -> Quaternion
multv q v = hamilton q (fromVec3 v)

{-| Multiplication of a vector by a quaternion -}
vmult : Vec3 -> Quaternion -> Quaternion
vmult v q = hamilton (fromVec3 v) q

{-| Construct a quaternion from an orientation vector -}
orient : Vec3 -> Quaternion
orient v = normalize (fromVec3 v)
-- orient v = normalize (conjugate (fromVec3 v))

{-| Angle of rotation.
Returns angle in radians, in the range [0, 2*pi)
-}
getAngle : Quaternion -> Float
getAngle q = 2.0 * acos (getScalar q)

{-| Unit vector along the axis of rotation -}
getAxis : Quaternion -> Vec3
getAxis q = V3.normalize (toVec3 q)

{-| Quaternion from two vectors -}
fromTo : Vec3 -> Vec3 -> Quaternion
fromTo v1 v2 =
    let d2 l1 l2 = sqrt (l1*l1 * l2*l2) in
    normalize <| fromSV (d2 (V3.length v1) (V3.length v2) + V3.dot v1 v2, V3.cross v1 v2)

{-| Quaternion from two vectors -}
-- http://lolengine.net/blog/2014/02/24/quaternion-from-two-vectors-final
fromTo2 : Vec3 -> Vec3 -> Quaternion
fromTo2 u v =
    let norm_u_norm_v = sqrt (V3.dot u u * V3.dot v v)
        real_part0 = norm_u_norm_v + V3.dot u v
        (real_part, w) =
            if real_part0 < 1e-6 * norm_u_norm_v then
                let w =
                    if abs (V3.getX u) > abs (V3.getZ u) then
                        vec3 -(V3.getY u) (V3.getX u) 0
                    else
                        vec3 0 -(V3.getZ u) (V3.getY u)
                in (0,  w)
            else
                (real_part0, V3.cross u v)

    in normalize <| fromSV (real_part, w)

{-| Rotate quaternion q1 by quaternion q2 -}
rotate : Quaternion -> Quaternion -> Quaternion
rotate q1 q2 = hamilton q1 (hamilton q2 (conjugate q1))

{-| Rotate a vector v by the unit quaternion q -}
vrotate : Quaternion -> Vec3 -> Vec3
-- vrotate q v = toVec3 <| hamilton (multv q v) (conjugate q)
-- vrotate q v = toVec3 <| hamilton q (vmult v (conjugate q))
vrotate q v = toVec3 <| hamilton q (hamilton (fromSV (0, v)) (conjugate q))

{-| Construction of a right quaternion -}
fromAngleAxis : Float -> Vec3 -> Quaternion
fromAngleAxis twoTheta v =
    let {x,y,z} = V3.toRecord v
        theta = twoTheta / 2.0
        c = cos theta
        s = sin theta
    in vec4 c (x*s) (y*s) (z*s)

{-| Construction a unit quaternion from Tait-Bryan angles representing
(yaw, pitch, roll)

https://en.wikipedia.org/wiki/Euler_angles#Tait.E2.80.93Bryan_angles
-}
fromYawPitchRoll : (Float, Float, Float) -> Quaternion
fromYawPitchRoll (yaw, pitch, roll) =
  let
    cYaw = cos (yaw/2)
    sYaw = sin (yaw/2)
    cRoll = cos (roll/2)
    sRoll = sin (roll/2)
    cPitch = cos (pitch/2)
    sPitch = sin (pitch/2)

    q0 = cYaw * cRoll * cPitch + sYaw * sRoll * sPitch
    q1 = cYaw * sRoll * cPitch - sYaw * cRoll * sPitch
    q2 = cYaw * cRoll * sPitch + sYaw * sRoll * cPitch
    q3 = sYaw * cRoll * cPitch - cYaw * sRoll * sPitch

  in quaternion q0 q1 q2 q3

{-| Convert a unit quaternion to Yaw, Pitch, Roll
http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToEuler/index.htm
-}
toYawPitchRoll : Quaternion -> (Float, Float, Float)
toYawPitchRoll q =
    let 
        (q0, q1, q2, q3) = toTuple q

        q2q2 = q2*q2

        t = q2q2 + q3*q3

        (yaw, pitch, roll) =
            if (t > 0.499) then
                -- Singularity at north pole
                (2 * atan2 q1 q0, pi/2, 0)
            else if (t < -0.499) then
                -- Singularity at south pole
                (-2 * atan2 q1 q0, -pi/2, 0)
            else
                let
                    r0 = 2 * (q0*q1 + q2*q3)
                    r1 = 1 - 2*(q1*q1 + q2q2)
                    roll_ = atan2 r0 r1
        
                    t2_0 = 2 * (q0*q2 - q3*q1)
                    t2_1 = if t2_0 > 1.0 then 1.0 else t2_0
                    t2 = if t2_1 < -1.0 then -1.0 else t2_1
                    pitch_ = asin t2
        
                    t3 = 2.0 * (q0*q3 + q1*q2)
                    t4 = 1 - 2.0 * t
                    yaw_ = atan2 t3 t4
                in (yaw_, pitch_, roll_)

    in (yaw, pitch, roll)

{-| Convert to a Mat4 -}
toMat4 : Quaternion -> M4.Mat4
toMat4 q =
    let
        (yaw, pitch, roll) = toYawPitchRoll q
    in
        M4.makeRotate yaw V3.k
        |> M4.rotate pitch V3.i
        |> M4.rotate roll V3.j
