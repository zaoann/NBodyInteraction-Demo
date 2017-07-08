{-# LANGUAGE Arrows, FlexibleInstances #-}

module GLsetup where

import Control.Monad.Reader hiding (when)
import qualified Control.Monad as Mo (when)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.IORef

import Graphics.UI.GLUT
import qualified Graphics.UI.GLFW as FW

import Control.Wire
import FRP.Netwire
import Prelude hiding ((.), id, until)

import System.Exit
import System.IO
import Control.Concurrent (threadDelay)

type RealSample = GLdouble
--type RealSample = GLfloat

type Tuple3 a = (a, a, a)
--type GL3Floats = (GLfloat, GLfloat, GLfloat)
--type GL3Doubles = (GLdouble, GLdouble, GLdouble)
type RealVec3 = Tuple3 RealSample
type ForceType = String
type InteractionMap = Map ForceType TypedInteractionField 
type ForceCouplings = Map ForceType (RealSample, (RealSample, RealSample))

type ParticleDynamics = (RealVec3, RealVec3)
type ParticleState = (RealSample, ParticleDynamics, ForceCouplings)
--type RelativeParticleState = ParticleState
type InteractionField = ParticleState -> ParticleState -> RealVec3
type TypedInteractionField = ForceType -> InteractionField
type ForceField = ParticleState -> RealVec3
--type ForceFields = [(ForceType, InteractionField)]

type CameraTripod = Tuple3 RealVec3
                    -- (eye position, direction, orientation(ie which way is up))
type CameraLens = (RealSample, RealSample, RealSample, RealSample)
                    -- (near, far, FoV in degrees, aspect ratio)
type Camera = (CameraTripod, CameraLens)
--type FirstPersonPerspective = (Camera, (Maybe RealSample, Maybe RealSample, Maybe RealSample))
defaultCamera = (((0,0,0), (0,0,-100), (0,1,0)), (0.01, 100, 90, fromIntegral(16)/fromIntegral(9)))
                    :: Camera

instance (Num a) => Num (a,a,a) where
    (x1,y1,z1) + (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
    (x1,y1,z1) * (x2,y2,z2) = (x1*x2, y1*y2, z1*z2)
    abs (x,y,z) = (abs x, abs y, abs z)
    signum (x,y,z) = (signum x, signum y, signum z)
    fromInteger n = (fromInteger n, fromInteger n, fromInteger n)
    negate (x,y,z) = (negate x, negate y, negate z)

(*.) :: Num a => a -> Tuple3 a -> Tuple3 a
a *. (x,y,z) = (a,a,a) * (x,y,z)

(./) :: Fractional a => Tuple3 a -> a -> Tuple3 a
(x,y,z) ./ a = (x/a, y/a, z/a)

dot :: Num a => Tuple3 a -> Tuple3 a -> a
dot v1 v2 = let (x,y,z) = v1*v2 in x+y+z

normSquared :: Num a => Tuple3 a -> a
normSquared (x,y,z) = x^2 + y^2 + z^2

norm :: Floating a => Tuple3 a -> a
norm = sqrt . normSquared

hat :: Floating a => Tuple3 a -> Tuple3 a
hat v = v./(norm v)

derivative_2 :: (HasTime t s, Monoid e, Monad m, RealFloat a) => Wire s e m (a,a) (a,a)
derivative_2 = proc (x,y) -> do
    x' <- derivative -< x
    y' <- derivative -< y
    returnA -< (x',y')
    

integral_3 :: (HasTime t s, Monad m, Fractional a) =>
            (a,a,a) -> Wire s e m (a,a,a) (a,a,a)
integral_3 (x0,y0,z0) = proc (x,y,z) -> do
    xx <- integral x0 -< x
    yy <- integral y0 -< y
    zz <- integral z0 -< z
    returnA -< (xx,yy,zz)


points :: Int -> [RealVec3]
points n = [ r*.(sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
    where
        n' = realToFrac n
        r = (n'-1)/10

points' :: Int -> [RealVec3]
points' n = [ (-cos (2*pi*k/n'), sin (2*pi*k/n'), 0) | k <- [1..n'] ]
    where n' = realToFrac n

particlesOnCircle :: Int -> [ParticleState]
particlesOnCircle n = zip3  (let ones = 1:ones in ones)
                            ((points n) `zip` (map ((0.9,0.9,0.9)*) (points' n)))
--                            ((points n) `zip` (map ((0,0,0)*) (points' n)))
                            (let frc = (M.fromList [("Gravity",(1,(0.1,100))),
                                                     ("Elasticity",(1,(0,0.2)))
                                                    ]):frc in frc)

toVector3 :: Num a => Tuple3 a -> Vector3 a
toVector3 (x,y,z) = Vector3 x y z 

toVertex3 :: Num a => Tuple3 a -> Vertex3 a
toVertex3 (x,y,z) = Vertex3 x y z 

setCamera :: Camera -> IO ()
setCamera cam @((eye, at, up), (near, far, fov, aspect)) = do
    matrixMode $= Projection
    loadIdentity
    let phi = (fov*pi)/360
        top = near / (cos(phi)/sin(phi))
        right = top * aspect
    frustum (-right) right (-top) top near far
    matrixMode $= Modelview 0
    loadIdentity
    lookAt (toVertex3 eye) (toVertex3 at) (toVector3 up)


reshape :: ReshapeCallback
reshape winSize@(Size w h) = do
    viewport $= (Position 0 0, winSize)
--    setCamera defaultCamera
    {-
    matrixMode $= Projection
    loadIdentity
    let near = 0.01
        far = 100
        fov = 90
        phi = (fov*pi)/360
        top = near/(cos(phi)/sin(phi))
        aspect = fromIntegral(w)/fromIntegral(h)
        right = top*aspect
    frustum (-right) right (-top) top near far
    matrixMode $= Modelview 0
    loadIdentity
    -}
    postRedisplay Nothing

color3f :: RealVec3 -> IO ()
color3f (r, g, b) = color $ Color3 r g b

vertex3f :: RealVec3 -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

scale3f :: RealVec3 -> IO ()
scale3f (x, y, z) = scale x y z

scale1f :: RealSample -> IO ()
scale1f z = scale z z z

initGLUT :: IO ()
initGLUT = do
--    blend $= Enabled
--    lineSmooth $= Enabled
--    pointSmooth $= Enabled
--    polygonSmooth $= Enabled

    initialWindowSize $= Size 1280 720
    
errorCallback :: FW.ErrorCallback
errorCallback err description = hPutStrLn stderr description

initGLFW :: IO FW.Window
initGLFW = do
    FW.setErrorCallback (Just errorCallback)
    ok <- FW.init
    if not ok
        then do
            fail "Failed to initialize GLFW."
            exitFailure
        else do
--            mapM_ FW.windowHint [ FW.WindowHint'Samples 4    -- 4x antialiasing
--                                , FW.WindowHint'ContextVersionMajor 3    -- OpenGL 3.3
--                                , FW.WindowHint'ContextVersionMinor 3
--                                , FW.WindowHint'OpenGLProfile FW.OpenGLProfile'Core
--                                ]
            FW.defaultWindowHints
            newWin <- FW.createWindow 1280 720 "Hello Window" Nothing Nothing
            case newWin of
                Nothing -> do
                    _ <- fail "Failed to create OpenGL window."
                    FW.terminate
                    exitFailure
                Just win -> do
                    FW.makeContextCurrent newWin
--                    FW.setStickyKeysInputMode win FW.StickyKeysInputMode'Enabled
--                    FW.setCursorInputMode win FW.CursorInputMode'Disabled
                    depthFunc $= Just Less
                    return win

wireLoop :: FW.Window ->
    Session IO s ->
--    Wire s e (ReaderT KeyboardMouseInput Identity) a ([ParticleState], Camera) ->
    Wire s e IO a ([ParticleState], Camera) ->
    IO ()
wireLoop win s0 w0 = do
    FW.pollEvents
    mbti <- FW.getTime      -- get time before rendering.
    ti <- case mbti of
            Nothing -> fail "GLFW failed to getTime 0."
            Just jt -> return jt

    (t0, s1) <- stepSession s0
--    let (currState, w1) = runReader (stepWire w0 t0 (Right undefined)) kbInput
    (currState, w1) <- stepWire w0 t0 (Right undefined)
    
    case currState of
        Right (ps, cam) -> display ps cam
        Left _ -> return ()     -- OK desuka?

    FW.swapBuffers win
    keyState <- FW.getKey win FW.Key'Escape
    closeWindow <- FW.windowShouldClose win

    case (keyState, closeWindow) of
        (FW.KeyState'Released, False) -> do mbtf <- FW.getTime  -- get time after rendering.
                                            tf <- case mbtf of
                                                    Nothing -> fail "GLFW failed to getTime 1."
                                                    Just jt -> return jt
                                            let dt = ti + spf - tf
                                            Mo.when (dt > 0) $ threadDelay (truncate $ 1000000 * dt)
                                            wireLoop win s1 w1
        _ -> return ()
    where
        fps = 60
        spf = recip fps


display :: [ParticleState] -> Camera -> DisplayCallback
display currState cam = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity    -- resets modification matrix
    setCamera cam
--    setCamera defaultCamera
--    setCamera (((10,0,0), (0,0,-100), (0,1,0)), (0.01, 100, 90, 16/9))

    mapM_ go currState

--    swapBuffers
--    FW.swapBuffers win
--    displayCallback $= display nextSession nextWire
    where
        go (c, ((x,y,z), _), _) = preservingMatrix $ do
--        go (c, ((x,y,z), _)) = do
--            loadIdentity
            color3f ((x+1)/2, (y+1)/2, (z+1)/2)
            translate $ Vector3 0 0 (-5::RealSample)
            translate $ Vector3 x y z
            renderObject Wireframe (Sphere' 0.2 15 10)

display' :: DisplayCallback
display' = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity    -- resets modification matrix
    setCamera (((0,0,0), (0,0,100), (0,0,1)), (0.01, 100, 90, 16/9))
    setCamera defaultCamera
--    matrixMode $= Modelview 0
--    loadIdentity

    renderObject Wireframe (Sphere' 0.2 15 10)

    swapBuffers

--idle :: (Real t) => Session IO (Timed t ()) -> Wire (Timed t ()) () IO a [ParticleState]
--           -> DisplayCallback
--idle :: Session IO (Timed t ()) -> Wire (Timed t ()) () IO a [ParticleState]
--            -> IORef (Wire (Timed t ()) () IO a [ParticleState])
--            -> IORef (Wire (Timed t ()) () IO a ())
--            -> DisplayCallback
idle kbRef s w = do
    kbInput@(kbMap, cursorPos) <- readIORef (kbRef :: IORef KeyboardMouseInput)
--    cursorPos <- readIORef msRef
    (currSession, nextSession) <- stepSession s
--    let w' = keyHitAt (Char ' ', noMod) <|> w
    let (currState, nextWire) = runReader (stepWire w currSession (Right undefined)) kbInput
--    let w' = b <|> w
--    angle $~! (+d)
--    (Right currState, nextWire) <- stepWire w currSession (Right undefined)
--    writeIORef wRef nextWire
    
    case currState of
--        Right st -> displayCallback $= display st
        Right (ps, cam) -> displayCallback $= display ps cam
        Left _ -> return ()
--    keyboardMouseCallback $= Just (keyboardMouse nextSession nextWire)
    idleCallback $= Just (idle kbRef nextSession nextWire)
    postRedisplay Nothing

--cameraMovement :: (HasTime t s, Monoid e) => Wire s e (Reader KeyboardMouseInput) a Camera
cameraMovement :: (HasTime t s, Monoid e) => FW.Window -> Wire s e IO a Camera
cameraMovement win = proc x -> do
    tripodPos <- integral_3 (0,0,0) . (speed win) -< x
    (cx', cy') <- mouseClick win (FW.MouseButton'2) . derivative_2 . cursorPos win <|> pure (0,0)-< x
    theta <- integral (3*pi/2) -< realToFrac $ cx' * 2*pi/w
    phi <- integralWithin (pi/2) (0,pi) -< realToFrac $ cy' * pi/h
    tripodDir <- returnA -< (sin(phi)*cos(theta), cos(phi), sin(phi)*sin(theta)) + tripodPos
            -- flickers at North and South Pole... how to deal with it?
--    tripodDir <- pure (0,0,-100) -< x
    tripodUp <- pure (0,1,0) -< x
    near <- pure 0.01 -< x
    far <- pure 100 -< x
    fov <- pure 90 -< x
--    phi = (fov*pi)/360
--    top = near/(cos(phi)/sin(phi))
    aspect <- pure (realToFrac w / realToFrac h) -< x
--    right = top*aspect
    returnA -< ((tripodPos,tripodDir,tripodUp), (near, far, fov, aspect))
    where
        v = 5
        w = 1280
        h = 720
        go win k vel = (pure vel) . keyDown win k <|> pure (0,0,0)
        speed win = go win FW.Key'Up (0,0,-v) + go win FW.Key'Down (0,0,v) +
                    go win FW.Key'Right (v,0,0) + go win FW.Key'Left (-v,0,0) 

integralWithin :: (HasTime t s, MonadFix m, Ord a, Fractional a) => a -> (a,a) -> Wire s e m a a
integralWithin x0 (inf, sup) = proc x' -> do
    rec
        let xx' = if (x<inf+0.01) && (x'<0)
                    then 0
                    else if (x>sup-0.01) && (x'>0)
                    then 0
                    else x'
        x <- delay x0 . integral x0 -< xx'
    returnA -< x
            

anyCursorPos :: Position -> Bool
anyCursorPos _ = True
    
switchUntil :: (HasTime t s, Monoid e, Monad m) =>
                Wire s e m a b -> Wire s e m a (Event b) -> Wire s e m a b -> Wire s e m a b
switchUntil w0 we w1 = until . (w0 &&& we) --> w1

repeatUntil :: (HasTime t s, Monoid e, Monad m) =>
                Wire s e m a b -> Wire s e m a (Event b) -> Wire s e m a b
--repeatUntil w we = until . (w &&& we) --> repeatUntil w we
repeatUntil w we = switchUntil w we (repeatUntil w we)

repeatFor :: (HasTime t s, Monoid e, Monad m) => t -> Wire s e m a b -> Wire s e m a b
repeatFor t w = (for t . w) --> repeatFor t w

periodicHold :: (HasTime t s, Monoid e, Monad m, Fractional t) => t -> Wire s e m a a
periodicHold t = holdFor (t/2) . periodic t

--keyboardMouse :: (Real t) => Session IO (Timed t ()) -> Wire (Timed t ()) () IO a [ParticleState]
--                    -> KeyboardMouseCallback
keyboardMouse :: IORef KeyboardMouseInput -> KeyboardMouseCallback
keyboardMouse currInputRef key keySt modifier mousePos
    = case (key, keySt, shift modifier, ctrl modifier, alt modifier) of
        ((Char '\ESC'), Down, _, _, _) -> leaveMainLoop
        (k, Down, sSt, cSt, aSt) -> do
                            (ks, mp) <- readIORef currInputRef
                            writeIORef currInputRef ((M.insert k (sSt, cSt, aSt) ks), mousePos)
                                -- mousePos or mp??
        (k, Up, _, _, _) -> do
                            (ks, mp) <- readIORef currInputRef
                            writeIORef currInputRef ((M.delete k ks), mousePos)

--    (Char ' ') -> idleCallback $= Just (idle s ((for 1) . w --> w))
--    (Char ' ') -> writeIORef b $ mkPure_ (\_ -> Right ())
--    _ -> do
--            writeIORef b $ mkPure_ (\_-> Left ())
--            return ()
--    where
--        w' = (repeatUntil w now)

pressedMotion :: IORef KeyboardMouseInput -> MotionCallback
pressedMotion kmRef cursorNow = do
    (k, m) <- readIORef kmRef
    writeIORef kmRef (k, cursorNow)


type Input = (Set FW.Key, Set FW.MouseButton, CursorPos)
type CursorPos = (Double, Double)

keyDown :: Monoid e => FW.Window -> FW.Key -> Wire s e IO a a       -- ReaterT FW.Window IO ?
keyDown win k = mkGen_ $ \x -> do
                        ks <- FW.getKey win k
                        case ks of
                            FW.KeyState'Pressed -> return $ Right x
                            FW.KeyState'Repeating -> return $ Right x
                            _ -> return $ Left mempty

keyToggle :: Monoid e => FW.Window -> FW.Key -> Bool -> Wire s e IO a a
keyToggle win k b = mkGenN $ \x -> do               -- b is the initial condition, True = on.
                                ks <- FW.getKey win k
                                case (ks, b) of
                                    (FW.KeyState'Pressed, False) ->
                                        return (Right x, keyToggle win k True)
                                    (FW.KeyState'Pressed, True) ->
                                        return (Left mempty, keyToggle win k False)
                                    (_, False) ->       -- ignore KeyState'Repeating for now.
                                        return (Left mempty, keyToggle win k b)
                                    (_, True) ->
                                        return (Right x, keyToggle win k b)

mouseClick :: Monoid e => FW.Window -> FW.MouseButton -> Wire s e IO a a
mouseClick win m = mkGen_ $ \x -> do
                        ms <- FW.getMouseButton win m
                        case ms of
                            FW.MouseButtonState'Pressed -> return $ Right x
                            _ -> return $ Left mempty


cursorPos :: Monoid e => FW.Window -> Wire s e IO a CursorPos
cursorPos win = mkGen_ $ \x -> do
                        cp <- FW.getCursorPos win
                        return $ Right cp

cursorAt :: Monoid e => FW.Window -> (CursorPos -> Bool) -> Wire s e IO a a
cursorAt win f = mkGen_ $ \x -> do
                        cp <- FW.getCursorPos win
                        case f cp of
                            True -> return $ Right x
                            False -> return $ Left mempty


--keyPressedAt :: Monoid e => FW.Key -> (CursorPos -> Bool) -> Wire s e (Reader Input) a a
--keyPressedAt k f = (keyPressed k) . (cursorAt f)


type ModifierCombo = (KeyState, KeyState, KeyState) -- shift, ctrl, alt
type KeyCombo = (Key, ModifierCombo)
type KeyMap = Map Key ModifierCombo
type KeyboardMouseInput = (KeyMap, Position)

noMod = (Up,Up,Up) :: ModifierCombo

keyHitAt :: Monoid e => KeyCombo -> (Position -> Bool) -> Wire s e (Reader KeyboardMouseInput) a a
keyHitAt (k, km) mRange = mkGen_ (\x -> do
                                (ks, mp) <- ask
                                if ((k `M.lookup` ks) == Just km) && (mRange mp)
                                    then return (Right x)
                                    else return (Left mempty)
                            )

cursorPosition :: Monoid e => Wire s e (Reader KeyboardMouseInput) a (GLint, GLint)
cursorPosition = mkGen_ (\_ -> do
                    (k, Position x y) <- ask
                    return (Right (x, y))
                )



test1 :: (Show c, Num a) => (c -> a) -> Wire s e (ReaderT c IO) a a
test1 f = mkGen_ (\x -> do
                    y <- ask
                    ys <- asks $ (\y' -> (y':[y]))  -- 'asks' doesn't need to take (a -> IO b)
                    x' <- asks $ f
                    return (Right (x+x'))
                )

-- | m is a monad transformer with MonadIO instance, think ReaderT.
-- | Applicative m is required from clockSession_.
-- | 
testWire_clock :: (MonadIO m, Applicative m) => (e -> IO c) -> (b -> IO c)
                    -> Wire (Timed NominalDiffTime ()) e m a b -> m d
testWire_clock inhibitAction runningAction wire = loop wire clockSession_
    where
        loop w s = do
            (currSession, nextSession) <- stepSession s
            (v, nextWire) <- stepWire w currSession (Right undefined)
            case v of
                Left e -> liftIO $ inhibitAction e
                Right b -> liftIO $ runningAction b
            loop nextWire nextSession
{-
main :: IO ()
main = do
    runReaderT (testWire_clock (return) (putStrLn . show) (when (<11.1) . test1 (+1). time)) (10)
-}
