module Primitives where
import Graphics.Rendering.OpenGL as OpenGL

data (Num a) => Vector a = Vector a a

add (Vector xA yA) (Vector xB yB) = Vector (xA + xB) (yA + yB)
sub (Vector xA yA) (Vector xB yB) = Vector (xA - xB) (yA - yB)
opposite (Vector a b) = Vector (-a) (-b)
value (Vector x y) = sqrt $ x**2 + y**2
normalize' vector@(Vector x y) = Vector (x/len) (y/len) where len = value vector
scalarMult z (Vector x y) = Vector (x*z) (y*z)

type Position = Vector Float
type Velocity = Vector Float
type Force = Vector Float

prepRenderPos :: Primitives.Position -> Vertex3 Float
prepRenderPos (Vector x y) = OpenGL.Vertex3 x y 0

instance (Num a) => Show (Vector a) where
	show (Vector x y) = "(" ++ show x ++ "," ++ show y ++ ")"

x -: f = f x

sampleVector1 = Vector 1.0 1.0
sampleVector2 = Vector 2.0 2.0