import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import ParticlesConfig

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
	addTimerCallback 25 $ drawNext (points + 1)

	mainLoop

display = do
	clear [ColorBuffer]
	renderPrimitive Points $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) myPoints
	flush

drawNext steps = do
	clear [ColorBuffer]
	renderPrimitive Points $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) $ genPoints [-steps..steps]
	addTimerCallback 5 $ drawNext (steps + 1)
	flush

reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	postRedisplay Nothing