module World where
import Graphics.Rendering.OpenGL
import ParticlesConfig
import Primitives

data CelestialObject = CelestialObject {
	getRadius :: Float,
	getPosition :: Primitives.Position,
	getVelocity :: Velocity
}
instance Show CelestialObject where
	show (CelestialObject r position velocity) =
		"(Celestial r=" ++ show r ++ " " ++ show position ++ " v=" ++ show velocity ++ ")"

getMass (CelestialObject r _ _) = 4/3 * pi * (r**3)
distanceBetween a b = value $ sub (getPosition a) (getPosition b)

--f=G(M*m)/(r^2)
gravConst  = 1

--forceBetween :: (CelestialObject a) => a -> a -> Force
scalarForceBetween celA celB =
	let
		massA = getMass celA
		massB = getMass celB
	in gravConst * (massA * massB) / (distanceBetween celA celB)

--from A to B
vectorForceBetween celA celB =
	let
		posA = getPosition celA
		posB = getPosition celB
	in sub posB posA

vectorBetween celA celB =
	let
		normalized = normalize' $ vectorForceBetween celA celB
		factor = scalarForceBetween celA celB
	in scalarMult factor normalized

applyVelocity (CelestialObject r position velocity) =
	CelestialObject r (add position velocity) velocity

prepRenderCelestial (CelestialObject _ position _) = prepRenderPos position
scaleByBounds (Vertex3 x y z) = Vertex3 (x/bound) (y/bound) (z/bound)

data State = State [CelestialObject] deriving (Show)
tick (State cels) = State $ map applyVelocity cels

prepRenderState :: State -> [Vertex3 Float]
prepRenderState (State cels) = map (scaleByBounds.prepRenderCelestial) cels

sampleCell1 = CelestialObject 1 (Vector 0 0) (Vector 10 10)
sampleCell2 = CelestialObject 1 (Vector 0 0) (Vector (-10) 10)
sampleCell3 = CelestialObject 1 (Vector 0 0) (Vector 10 (-10))
sampleCell4 = CelestialObject 1 (Vector 0 0) (Vector (-10) (-10))

sampleCell10 = CelestialObject 1 (Vector 0 0) (Vector 0 10)
sampleCell20 = CelestialObject 1 (Vector 0 0) (Vector 10 0)
sampleCell30 = CelestialObject 1 (Vector 0 0) (Vector 0 (-10))
sampleCell40 = CelestialObject 1 (Vector 0 0) (Vector (-10) 0)
sampleState1 = State [sampleCell1, sampleCell2, sampleCell3, sampleCell4,
	sampleCell10, sampleCell20, sampleCell30, sampleCell40]

gravCell1 = CelestialObject 1 (Vector 10 0) (Vector 0 0)
gravCell2 = CelestialObject 1 (Vector (-10) 0) (Vector 0 0)

--TODO
--1. Set world bounds
--2. PoC Render moving celestials
--3. Ticking world physics