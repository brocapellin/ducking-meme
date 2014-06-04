import Graphics.UI.GLUT
import Foreign.C.Types
import Data.Fixed
import Data.IORef

main = do
    getArgsAndInitialize
    setupWindow
    renderAtFPS 25
    animatedTriangle
    mainLoop

setupWindow = do
    createWindow "Hello World"
    windowSize $= Size 640 480

animatedTriangle = do
    runtimeRef <- newIORef 0.0
    keyboardCallback $= toKbdCb animatedTriangle
    displayCallback $= do
        clear [ ColorBuffer ]
        renderPrimitive Triangles $ do
            runtime <- get runtimeRef
            colorPoint (calculateColor runtime red) left bottom
            colorPoint (calculateColor runtime green) right bottom
            colorPoint (calculateColor runtime blue) middle top
        flush
    runWorld runtimeRef
    where
        runWorld runtimeRef = addTimerCallback (1000 `div` 40) $ do
            runtime <- get runtimeRef
            runtimeRef $= runtime*1.003 + 1.0 / 40 -- :)
            runWorld runtimeRef

renderAtFPS fps = addTimerCallback (1000 `div` fps) $ do
    maybeWindow <- get currentWindow
    postRedisplay maybeWindow
    renderAtFPS fps

colorPoint col x y = do
    currentColor $= col
    vertex $ Vertex2 (CInt x) (CInt y)

calculateColor time (Color4 r g b a) 
    | increaseR = Color4 (clamp (r'+dt)) g' (clamp (b'-dt)) a
    | increaseG = Color4 (clamp (r'-dt)) (clamp (g'+dt)) b' a
    | increaseB = Color4 r' (clamp (g'-dt)) (clamp (b'+dt)) a
    where increaseR = b' > 0.0 && g' < 0.0001
          increaseG = r' > 0.0 && b' < 0.0001
          increaseB = g' > 0.0 && r' < 0.0001
          clamp = min 1.0 . max 0.0
          dt = time `mod'` 1.0
          dt3 = time `mod'` 3.0
          (r',g',b') | dt3 < 1.0 = (r,g,b)
                     | dt3 < 2.0 = (b,r,g)
                     | otherwise = (g,b,r)

bottom = -1
left = -1
top = 1
right = 1
middle = 0

red   = Color4 1.0 0.0 0.0 1.0
green = Color4 0.0 1.0 0.0 1.0
blue  = Color4 0.0 0.0 1.0 1.0

toKbdCb io = Just (\_ _ -> io)
