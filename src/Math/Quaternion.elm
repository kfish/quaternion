module Math.Quaternion exposing (..)

{-|

Quaternions

# Types
@docs Quaternion

# Create
@docs unit, quaternion, fromAngleAxis, fromEuler, fromRecord, fromSV, fromScalar, fromTo, fromTo2, fromTuple, fromVec3, orient

# Get and Set
@docs getI, getJ, getK, getScalar, getAngle, getAxis, setI, setJ, setK, setScalar

# Operations
@docs length, lengthSquared, normalize, negate, scale, add, sub, conjugate, hamilton, vmult, vrotate, multv, rotate


# Conversions
@docs toEuler, toMat4, toRecord, toSV, toTuple, toVec3

-}

import Math.Matrix4 as M4
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as V3
import Math.Vector4 exposing (Vec4, vec4)
import Math.Vector4 as V4

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

{-| Angle of rotation -}
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
        (real_part, w) =
            if real_part < 1e-6 * norm_u_norm_v then
                let w =
                    if abs (V3.getX u) > abs (V3.getZ u) then
                        vec3 -(V3.getY u) (V3.getX u) 0
                    else
                        vec3 0 -(V3.getZ u) (V3.getY u)
                in (0,  w)
            else
                (norm_u_norm_v + V3.dot u v, V3.cross u v)

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
        theta = twoTheta / 2
        c = cos theta
        s = sin theta
    in vec4 c (x*s) (y*s) (z*s)

{-| Construction from Euler angles representing (roll, pitch, yaw),
often denoted phi, tau, psi -}
fromEuler : (Float, Float, Float) -> Quaternion
fromEuler (phi, tau, psi) =
{-
    let
        roll  = quaternion (cos (phi/2)) 0 0 (sin (phi/2))
        pitch = quaternion (cos (tau/2)) (sin (tau/2)) 0 0
        yaw   = quaternion (cos (psi/2)) 0 (sin (psi/2)) 0
    -- in roll `hamilton` pitch `hamilton` yaw
    in yaw `hamilton` pitch `hamilton` roll
-}
    let
        sphi = sin (phi/2)
        cphi = cos (phi/2)
        stau = sin (tau/2)
        ctau = cos (tau/2)
        spsi = sin (psi/2)
        cpsi = cos (psi/2)

        s = cphi * ctau * cpsi + sphi * stau * spsi
        i = sphi * ctau * cpsi - cphi * stau * spsi
        j = cphi * stau * cpsi + sphi * ctau * spsi
        k = cphi * ctau * spsi - sphi * stau * cpsi
    in quaternion s j k i -- fromFlightDynamics
        
{-| Convert to Euler angles representing (roll, pitch, yaw),
often denoted (phi, tau, psi) -}
toEuler : Quaternion -> (Float, Float, Float)
toEuler q =
    let
        -- Translate from flight dynamics coordinate system
        (q0, q2, q3, q1) = toTuple q

        -- https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles#Euler_Angles_from_Quaternion
        phi = atan2 (2 * (q0*q1 + q2*q3)) (1 - 2 * (q1*q1 + q2*q2))
        tau = asin  (2 * (q0*q2 - q3*q1))
        psi = atan2 (2 * (q0*q3 + q1*q2)) (1 - 2 * (q2*q2 + q3*q3))
    in
        (phi, tau, psi)

{-| Convert to a Mat4 -}
toMat4 : Quaternion -> M4.Mat4
toMat4 q =
    let
        (phi, tau, psi) = toEuler q
    in
        -- M4.makeRotate (phi+pi) V3.k
        M4.makeRotate (phi) V3.k
        |> M4.rotate tau V3.i
        |> M4.rotate psi V3.j
{-
        M4.makeRotate psi V3.j
        |> M4.rotate tau V3.i
        |> M4.rotate phi V3.k
-}
