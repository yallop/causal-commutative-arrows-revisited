{-# LANGUAGE Arrows #-}
module Sample where

import Control.Arrow
import Control.CCA.Types
import Control.CCA.Instances
import SigFun
import Prelude hiding (init, exp)

sr = 44100 :: Int
dt = 1 / (fromIntegral sr)

exp :: ArrowInit a => a () Double
exp = proc () -> do
  rec let e = 1 + i
      i <- integral -< e
  returnA -< e
-- SPECIALIZE INLINE would make SF version even slower!
-- {-# SPECIALIZE INLINE exp :: SF () Double #-} 
{-# SPECIALIZE INLINE exp :: CCNF_D () Double #-}
{-# SPECIALIZE INLINE exp :: CCNF_ST s () Double #-}
integral :: ArrowInit a => a Double Double
integral = proc x -> do
  rec let i' = i + x * dt
      i <- init 0 -< i'
  returnA -< i

-- alternatively, we could define intergral directly as loopD,
-- which helps CCNF_ST signficantly by skip the direct use of
-- loop combinator, because loop uses mfix with a significant 
-- overhead.

-- integral = loopD 0 (\ (x, i) -> (i, i + dt * x)) 

sine :: ArrowInit a => Double -> a () Double
sine freq = proc _ -> do
  rec x <- init i -< r
      y <- init 0 -< x 
      let r = c * x - y
  returnA -< r
  where
    omh = 2 * pi / (fromIntegral sr) * freq
    {-# NOINLINE omh #-}
    i = sin omh
    {-# NOINLINE i #-}
    c = 2 * cos omh
    {-# NOINLINE c #-}
-- SPECIALIZE INLINE would make SF version even slower!
-- {-# SPECIALIZE INLINE sine :: Double -> SF () Double #-}
{-# SPECIALIZE INLINE sine :: Double -> CCNF_D () Double #-}
{-# SPECIALIZE INLINE sine :: Double -> CCNF_ST s () Double #-}

oscSine :: ArrowInit a => Double -> a Double Double
oscSine f0 = proc cv -> do
  let f = f0 * (2 ** cv)
  phi <- integral -< 2 * pi * f
  returnA -< sin phi

testOsc :: ArrowInit a => (Double -> a Double Double) -> a () Double
testOsc f = arr (\_ -> 1) >>> f 440

oscSineA :: ArrowInit a => a () Double
oscSineA = testOsc oscSine
-- SPECIALIZE INLINE would make SF version even slower!
-- {-# SPECIALIZE INLINE oscSineA :: SF () Double #-}
{-# SPECIALIZE INLINE oscSineA :: CCNF_D () Double #-}
{-# SPECIALIZE INLINE oscSineA :: CCNF_ST s () Double #-}

sciFi :: ArrowInit a => a () Double
sciFi = proc () -> do
   und <- oscSine 3.0 -< 0
   swp <- integral -< -0.25
   audio <- oscSine 440 -< und * 0.2 + swp + 1
   returnA -< audio
-- SPECIALIZE INLINE would make SF version even slower!
-- {-# SPECIALIZE INLINE sciFi :: SF () Double #-}
{-# SPECIALIZE INLINE sciFi :: CCNF_D () Double #-}
{-# SPECIALIZE INLINE sciFi :: CCNF_ST s () Double #-}

robot :: ArrowInit a => a (Double, Double) Double
robot = proc inp -> do
    let vr = snd inp
        vl = fst inp
        vz = vr + vl
    t <- integral -< vr - vl
    let t' = t / 10
    x <- integral -< vz * cos t'
    y <- integral -< vz * sin t'
    returnA -< x / 2 + y / 2

testRobot :: ArrowInit a => a (Double, Double) Double -> a () Double
testRobot bot = proc () -> do
    u <- sine 2 -< ()
    robot -< (u, 1 - u)

robotA :: ArrowInit a => a () Double
robotA = testRobot robot
-- SPECIALIZE INLINE would make SF version even slower!
-- {-# SPECIALIZE INLINE robotA :: SF () Double #-}
{-# SPECIALIZE INLINE robotA :: CCNF_D () Double #-}
{-# SPECIALIZE INLINE robotA :: CCNF_ST s () Double #-}

fibA :: ArrowInit arr => arr () Integer
fibA = proc _ -> do
   rec let r = d2 + d1
       d1 <- init 0 -< d2
       d2 <- init 1 -< r
   returnA -< r
-- SPECIALIZE INLINE would make SF version even slower!
-- {-# SPECIALIZE INLINE fibA :: SF () Integer #-}
{-# SPECIALIZE INLINE fibA :: CCNF_D () Integer #-}
{-# SPECIALIZE INLINE fibA :: CCNF_ST s () Integer #-}

