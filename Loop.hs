import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import ParticlesConfig
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
	displayCallback $= display
	reshapeCallback $= Just reshape

	--Step based animation
	addTimerCallback 25 $ drawNext (sampleState1 -: tick)

	mainLoop

display = do
	clear [ColorBuffer]
	renderPrimitive Points $ mapM_ (\v3->vertex$v3) $ sampleState1 -: prepRenderState
	flush

drawNext state = do
	clear [ColorBuffer]
	renderPrimitive Points $ mapM_ (\v3->vertex$v3) $ state -: prepRenderState
	addTimerCallback 5 $ drawNext (state -: tick)
	flush

reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	postRedisplay Nothing