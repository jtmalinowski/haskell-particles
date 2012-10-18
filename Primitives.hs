module Primitives where
import Graphics.Rendering.OpenGL as OpenGL

data (Num a) => Vector a = Vector a a

add (Vector xA yA) (Vector xB yB) = Vector (xA + xB) (yA + yB)
sub (Vector xA yA) (Vector xB yB) = Vector (xA - xB) (yA - yB)
value (Vector x y) = sqrt $ x**2 + y**2

type Position = Vector Float
type Velocity = Vector Float
type Force = Vector Float

prepRenderPos :: Primitives.Position -> Vertex3 Float
prepRenderPos (Vector x y) = OpenGL.Vertex3 x y 0

instance (Num a) => Show (Vector a) where
	show (Vector x y) = "(" ++ show x ++ "," ++ show y ++ ")"