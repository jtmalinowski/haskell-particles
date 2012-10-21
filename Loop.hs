import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import ParticlesConfig
import Primitives
import World

genPoints :: (Ord a, Floating a) => [a] -> [(a,a,a)]
genPoints xs =
	map (\k -> (k/len, thirdRoot (k/len), 0.0)) xs
	where len = ((/2).fromIntegral.length) xs

thirdRoot :: (Ord a, Floating a) => a -> a
thirdRoot x
	| isPositive =  result
	| otherwise = -result
	where
		isPositive = x >= 0
		result = (abs x) ** (1/3)

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = genPoints [-points..points]

main = do
	(progname, _) <- getArgsAndInitialize
	createWindow "Hello World"

	stateRef <- newIORef world1

	displayCallback $= display stateRef
	reshapeCallback $= Just reshape

	--Step based animation
	addTimerCallback 25 $ drawNext stateRef

	mainLoop

display stateRef = do
	clear [ColorBuffer]
	state <- readIORef stateRef
	renderCelestials state
	flush

drawNext stateRef = do
	clear [ColorBuffer]
	state <- readIORef stateRef
	renderCelestials state
	writeIORef stateRef $ state -: tick
	addTimerCallback 25 $ drawNext stateRef
	flush

renderCelestials state =
	let
		positions = state -: prepRenderState
		vertexes = mapM_ (\v3->vertex$v3) positions
	in renderPrimitive Points vertexes

reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	postRedisplay Nothing