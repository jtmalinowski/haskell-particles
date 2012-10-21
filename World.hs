module World where
import Debug.Trace
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
distanceBetween a b
	|	posA == posB = 2000000000.0
	| otherwise = len
	where
		posA = getPosition a
		posB = getPosition b
		len = value $ sub posA posB

--f=G(M*m)/(r^2)
gravConst  = 0.0001

--forceBetween :: (CelestialObject a) => a -> a -> Force
scalarForceBetween celA celB
	| distance < 5 = forceFactor / (5)
	| otherwise = forceFactor / distance
	where
		massA = getMass celA
		massB = getMass celB
		distance = distanceBetween celA celB
		forceFactor = gravConst * (massA * massB)

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

gravityBetween celA celB =
	let
		force = vectorBetween celA celB
		massA = getMass celA
		velocity = scalarMult (1.0/massA) force
	in velocity

applyVelocity (CelestialObject r position velocity) =
	CelestialObject r (add position velocity) (scalarMult 0.99 velocity)
applyAcceleration acceleration (CelestialObject r position velocity) =
	CelestialObject r position (add acceleration velocity)

prepRenderCelestial (CelestialObject _ position _) = prepRenderPos position
scaleByBounds (Vertex3 x y z) = Vertex3 (x/bound) (y/bound) (z/bound)

data State = State [CelestialObject] deriving (Show)
tick (State cels) = State $ monadComp cels
	where
		gravComp cel cells = do
			dest <- cells
			return $ gravityBetween cel dest
		monadComp cels = do
			cel <- cels
			let gravities = gravComp cel cels
			let gravity = foldl add (Vector 0.0 0.0) gravities
			let gravitedCel = applyAcceleration gravity cel
			return $ applyVelocity gravitedCel

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
--sampleState1 = State [sampleCell1, sampleCell2, sampleCell3, sampleCell4,
	--sampleCell10, sampleCell20, sampleCell30, sampleCell40]

gravCell1 = CelestialObject 1 (Vector 10 0) (Vector 0 0)
gravCell2 = CelestialObject 1 (Vector (-10) 0) (Vector 0 0)

planet1 = CelestialObject 10 (Vector (-50.0) 0.0) (Vector 0.0 0.0)
comet1 = CelestialObject 1 (Vector 100.0 100.0) (Vector 0.0 0.0)
comet2 = CelestialObject 1 (Vector 100.0 100.0) (Vector (-0.3) (-0.3))
world1 = State [planet1, comet1, comet2]

--TODO
--1. Set world bounds
--2. PoC Render moving celestials
--3. Ticking world physics
