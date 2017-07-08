{-# LANGUAGE Arrows, FlexibleInstances #-}

--import Control.Arrow
import Data.List (foldl', foldl1', transpose)
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.Fix
import Control.Monad.Reader hiding (when)
import Control.Wire
import FRP.Netwire
import Prelude hiding ((.), id, until)

import Graphics.UI.GLUT
import qualified Graphics.UI.GLFW as FW
import Data.IORef

import GLsetup

allInteractions :: InteractionMap
allInteractions = M.fromList [("Gravity", inversePowerInteraction 2 (-1)),
                        ("Elasticity", contactInteraction 1 1000)]

surfaceGravity :: RealSample -> RealSample -> RealSample -> ForceField
surfaceGravity g groudLevel groundElasticity p@(m,((x,y,z),q'),cpl) =
    case M.lookup "Elasticity" cpl of
        Just (k, (l,u)) -> bounce k u
        Nothing -> bounce 1 0
    where
        bounce k u = if (y-u) > groudLevel
                        then (0,-g*m,0)
                        else (0,groundElasticity*k*(groudLevel-y+u)-g*m,0)

boxBoundary :: (RealSample, RealSample, RealSample, RealSample, RealSample, RealSample) ->
                RealSample -> ForceField
boxBoundary (rightWall, leftWall, cover, floor, frontWall, backWall) k0 p@(m,((x,y,z),q'),cpl) =
    case M.lookup "Elasticity" cpl of
        Just (k, (l,u)) -> bounce k u
        Nothing -> bounce 1 0
    where
        bounce k u = (k0*k)*.(overBoundary u)
        overBoundary u = (heaviside (leftWall-x+u) - heaviside (x+u-rightWall),
                        heaviside (floor-y+u) - heaviside (y+u-cover),
                        heaviside (backWall-z+u) - heaviside (z+u-frontWall))
                         

heaviside :: RealSample -> RealSample
heaviside x
    | x <= 0 = 0
    | otherwise = x

dynamics :: (HasTime t s, MonadFix m) =>
        InteractionMap -> [ForceType] -> [ParticleState] -> Wire s e m [ForceField] [ParticleState]
dynamics fm fts ps0 = proc externalFields -> do
    rec
        let extForces = map (combineExternalForces externalFields) ps
        let intForces = combineInteractionForces fm fts ps
        let totForces = zipWith (+) extForces intForces
        ps <- (delay ps0) . systemDynamics ps0 -< totForces
    returnA -< ps

singleDynamics :: (HasTime t s, Monad m) =>
        ParticleState -> Wire s e m RealVec3 (ParticleState)
singleDynamics p0@(mass, (q_0, q_0'), forceCpls) = proc f -> do
    q' <- integral_3 q_0' -< f ./ mass
    q <- integral_3 q_0 -< q'
    returnA -< (mass, (q, q'), forceCpls)

systemDynamics :: (HasTime t s, Monad m) =>
        [ParticleState] -> Wire s e m [RealVec3] [ParticleState]
systemDynamics [] = mkSF_ (\_ -> [])
systemDynamics (qi_0 : qOthers_0) = proc (f:fOther) -> do
    qi <- singleDynamics qi_0 -< f
    qOthers <- systemDynamics qOthers_0 -< fOther
    returnA -< qi : qOthers

combineExternalForces :: [ForceField] -> ForceField
combineExternalForces [] _ = (0,0,0)
combineExternalForces (field:fs) p = (field p) + (combineExternalForces fs p)

combineInteractionForces :: InteractionMap -> [ForceType] -> [ParticleState] -> [RealVec3]
combineInteractionForces _ [] ps = repeat (0,0,0)
combineInteractionForces fm (ft:fts) ps = case lookupForce ft fm of
    Just f -> zipWith (+) (allPairs f [] ps) (combineInteractionForces fm fts ps)
    Nothing -> combineInteractionForces fm fts ps

allPairs :: InteractionField -> [[RealVec3]] -> [ParticleState] -> [RealVec3]
allPairs _ _ [] = []
allPairs f lls (p:ps) = let curr = (map (`f` p) ps)
                            (l,ls) = takeHeads lls
                            in (sum curr - sum l) : allPairs f (curr:ls) ps
    where
    takeHeads lls = (map head lls, map tail lls)


lookupForce :: ForceType -> InteractionMap -> Maybe InteractionField
lookupForce ft fm= case M.lookup ft fm of
    Just f -> Just $ f ft
    Nothing -> Nothing


inversePowerInteraction :: Int -> RealSample -> TypedInteractionField
inversePowerInteraction pwr cpl forceType (m1, (p1,p1'), fcpl1) (m2, (p2,p2'), fcpl2)
    = case (M.lookup forceType fcpl1, M.lookup forceType fcpl2) of
        (Just (c1,d1), Just (c2,d2)) -> if withinRange2 r d1 d2
            then let c = cpl * c1 * c2 in
                (c/r^(1+pwr)) *. r_
            else (0,0,0)
        _ -> (0,0,0)
    where
        r_ = p2-p1
        r = norm r_

contactInteraction :: Int -> RealSample -> TypedInteractionField
contactInteraction pwr cpl forceType (m1, (p1,p1'), fcpl1) (m2, (p2,p2'), fcpl2)
    = case (M.lookup forceType fcpl1, M.lookup forceType fcpl2) of
        (Just (c1,(l1,u1)), Just (c2,(l2,u2))) -> if withinRange2 r (l1,u1) (l2,u2)
            then let k = cpl * c1 * c2 in        -- k as in spring constant
                (k * (u1+u2-r)^(pwr)) *. (hat r_)       -- note it's penetratable, ie no infinity.
            else (0,0,0)
        _ -> (0,0,0)
    where
        r_ = p2-p1
        r = norm r_

withinRange :: Ord a => a -> (a,a) -> Bool
withinRange r (inf, sup) = (r <= sup) && (r > inf)

-- | logic: >inf if one's Center of Mass >inf of the other, <sup if boundary touches
withinRange2 :: (Ord a, Num a) => a -> (a,a) -> (a,a) -> Bool
withinRange2 r (inf1, sup1) (inf2, sup2) = withinRange r (min inf1 inf2, sup1+sup2)

withinCubeRange :: (Num a, Ord a) => (a,a,a) -> (a,a) -> Bool
withinCubeRange (x,y,z) (inf, sup)
    | any (>=sup) r = False
    | all (<inf) r = False
    | otherwise = True
    where
        r = map abs [x,y,z]

{-
forceFieldCombine :: (HasTime t s, Monad m) => InteractionField ->
    Wire s e m [ParticleState] (ParticleState -> RealVec3)
forceFieldCombine interaction = mkSF_ f
    where
        f [] = (\_ -> (0,0,0))
        f (p:ps) = fctAdd (interaction p) (f ps)    -- i.e. a right foldr'

fctAdd :: Num b => (a -> b) -> (a -> b) -> a -> b
fctAdd f g x = f x + g x
-}
{-
systemEvolve :: (HasTime t s, Monoid e) =>
--        [ParticleState] -> Wire s e (Reader KeyboardMouseInput) b [ParticleState]
        FW.Window -> [ParticleState] -> Wire s e IO b [ParticleState]
systemEvolve win ps_0 = proc _ -> do
    rec
        let q''s = map f qs
        qs <- (delay ps_0) . (systemDynamics ps_0) -< q''s
--        f <- (forceFieldCombine $ inverseSquareLaw 1) -< qs
--        f <- (keyHitAt (Char ' ', noMod) . pure (\_->(0,0,0))) <|> (forceFieldCombine $ inverseSquareLaw 1) -< qs
        f <-  (keyToggle win FW.Key'Enter False . pure (\_->(0,0,0))) <|>
                        (forceFieldCombine $ limitedToRange (inverseSquareLaw 1) (0,50)) -< qs
--                                            (forceFieldCombine $ (\x y -> (0,0,0))) -< qs
--        q''s <- delay zeroInitialAcceleration -< map f qs
            -- 'delay' will mess up the whole wire in inhibition if it takes inhibited wires
            -- from time 0. The initial value will be carried forward while inhibited.
            -- I think 'delay' should be time dependent 
    returnA -< qs
-}

{-
delayWire :: Session m t -> a -> Wire s e m a b -> Wire s e m a b
delayWire s x w0 = WPure $ \ _ mx -> case ms of
                                    Left ex -> (Left ex, w1)
                                    Right x' -> (Right x, delayWire x' w0)
                                        -- need to step s & w0... too lazy now, will be back..
-}

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize      -- glutInit
    win <- initGLFW
--    let w = ((dynamics allInteractions ["Gravity", "Elasticity"] $ particlesOnCircle 50).(pure [])::(HasTime t s) => Wire s () IO [ForceField] [ParticleState])
    let w = ((dynamics allInteractions ["Gravity","Elasticity"] $ particlesOnCircle 3).(pure [{-surfaceGravity 1 (-1) 1,-} boxBoundary (1,-1,1,-1,10,-10) 10])::(HasTime t s) => Wire s () IO [ForceField] [ParticleState])
--    wireLoop win (clockSession_) (w &&& cameraMovement win)
    wireLoop win (countSession_ (1/60)) (w &&& cameraMovement win)

    FW.terminate
    exit    -- deInitialize GLUT
    return ()

gravity1 :: ForceCouplings
gravity1 = M.singleton "Gravity" (1,(0,100))

initialCond :: [ParticleState]
initialCond' = [(1,((1,0,0),(1,0,0)), gravity1)]
initialCond'' = [(1,((0,1,0),(0,0,0)), gravity1), (1,((0,-1,0),(0,0,0)), gravity1)]
initialCond = [(1,((1,0,0),(0,0.5,0)), gravity1), (1,((-1,0,0),(0,-0.1,0)), gravity1)]
--initialCond = [(1,((1,0,0),(0,0.5,0))), (1,((-1,0,0),(0,-0.5,0))), (1,((1,1,-1),(-0.5,0.1,-0.2)))]

