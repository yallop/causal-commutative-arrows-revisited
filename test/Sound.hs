{-# LANGUAGE GeneralizedNewtypeDeriving, EmptyDataDecls, FlexibleInstances, FlexibleContexts, BangPatterns, ExistentialQuantification, ScopedTypeVariables, Arrows #-}
module Sound where

import Prelude hiding (init,id,(.))
import Control.Category
import Control.Arrow
import Control.CCA.Types
import Control.CCA.Instances
--import System.Random
import System.Random.Mersenne.Pure64
import System.IO.Unsafe
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import SoundAux
import SoundFun

outA :: Arrow a => a b b
outA = arr id

seghlp :: forall a p . (ArrowInit a, Clock p) =>
           [Double]  -- List of points to trace through.
        -> [Double]  -- List of durations for each line segment.
                     -- Needs to be one element fewer than 'iamps'.
        -> SigFun a p () (Double,Double,Double,Double)
seghlp iamps idurs =
    let sr = rate (undefined :: p)
        sz = length iamps
        amps = Tab iamps sz (listArray (0, sz-1) iamps)
        durs = Tab idurs (sz-1) (listArray (0, sz-2) (map (*sr) idurs))
    in proc _ -> do
      -- TODO: this is better defined using 'integral', but which is faster?
      rec
        let (t', i') = if t >= durs `aAt` i
                       then if i == sz-2 then (t+1, i) else (0, i+1)
                       else (t+1, i)
        i <- init 0 -< i'
        t <- init 0 -< t'
      let a1 = aAt amps i
          a2 = aAt amps (i+1)
          d  = aAt durs i
      outA -< (a1,a2,t,d)


envLine :: forall a p . (ArrowInit a, Clock p) =>
        Double  -- Starting value.
     -> Double  -- Duration in seconds.
     -> Double  -- Value after 'dur' seconds.
     -> SigFun a p () Double
envLine a dur b =
    let sr = rate (undefined :: p)
    in proc () -> do
      rec
        y <- init  a -< y + (b-a) * (1 / sr / dur)
      outA -< y


envLineSeg :: forall a p . (ArrowInit a, Clock p) =>
           [Double]  -- List of points to trace through.
        -> [Double]  -- List of durations for each line segment.
                     -- Needs to be one element fewer than 'amps'.
        -> SigFun a p () Double
envLineSeg amps durs =
    let sf = seghlp amps durs
    in proc () -> do
      (a1,a2,t,d) <- sf -< ()
      outA -< a1 + (a2-a1) * (t / d)


noiseWhite :: forall a p . (ArrowInit a, Clock p) => Int -> SigFun a p () Double
noiseWhite seed =
    let gen = pureMT $ fromIntegral seed 
    in proc () -> do
      rec
        let (a,g') = randomDouble g 
        g <- init gen -< g'
      outA -< a * 2 - 1


delayLine :: forall a p . (ArrowInit a, Clock p) =>
         Double -> SigFun a p Double Double
delayLine maxdel =
    let sr = rate (undefined :: p)
        sz = truncate (sr * maxdel)
        buf = mkArr sz
    in proc x -> do
        rec
          let i' = if i == sz-1 then 0 else i+1
          i <- init 0 -< i'
          y <- init 0 -< x
        outA -< unsafePerformIO $ updateBuf buf i y


butter :: forall a p . (ArrowInit a, Clock p) => SigFun a p (Double,ButterData) Double
butter = proc (sig, ButterData a1 a2 a3 a4 a5) -> do
    rec let t = sig - a4 * y' - a5 * y''
            y = t * a1 + a2 * y' + a3 * y''
        y'  <- init 0 -< t
        y'' <- init 0 -< y'
    outA -< y


filterLowPassBW :: forall a p . (ArrowInit a, Clock p) => SigFun a p (Double, Double) Double
filterLowPassBW =
  let sr = rate (undefined :: p)
  in proc (sig, freq) -> do
       butter -< (sig, blpset freq sr)


osc :: forall a p . (ArrowInit a, Clock p) =>
         Table
      -> Double  -- Initial phase of sampling, expressed as a
                 -- fraction of a cycle (0 to 1).
      -> SigFun a p Double Double
osc table iphs = osc_ iphs >>> readFromTableA table


readFromTableA :: Arrow a => Table -> a Double Double
readFromTableA = arr . readFromTable


osc_ :: forall a p . (ArrowInit a, Clock p) =>
          Double -> SigFun a p Double Double
osc_ phs =
    let sr = rate (undefined :: p)
    in proc freq -> do
      rec
        let delta = 1 / sr * freq
            phase = if next > 1 then frac next else next
        next <- init phs -< frac (phase + delta)
      outA -< phase

flute :: ArrowInit a => Time -> Double -> Double -> Double -> Double -> SigFun a AudRate () Double 
flute dur amp fqc press breath = 
  proc () -> do
    env1   <- envLineSeg  [0, 1.1*press, press, press, 0] 
                          [0.06, 0.2, dur-0.16, 0.02]  -< ()
    env2   <- envLineSeg  [0, 1, 1, 0] 
                          [0.01, dur-0.02, 0.01]       -< ()
    envib  <- envLineSeg  [0, 0, 1, 1] 
                          [0.5, 0.5, dur-1]            -< ()
    flow   <- noiseWhite 42    -< ()
    vib    <- osc sineTable 0  -< 5
    let  emb = breath*flow*env1 + env1 + vib*0.1*envib
    rec  flute  <- delayLine (1/fqc)    -< out
         x      <- delayLine (1/fqc/2)  -< emb + flute*0.4
         out    <- filterLowPassBW -< (x-x*x*x + flute*0.4, 2000)
    outA -< out*amp*env2
{-# SPECIALIZE INLINE flute :: Time -> Double -> Double -> Double -> Double -> SigFun CCNF_D AudRate () Double #-}
{-# SPECIALIZE INLINE flute :: Time -> Double -> Double -> Double -> Double -> SigFun SF AudRate () Double #-}

shepard :: ArrowInit a => Time -> SigFun a AudRate () Double 
shepard seconds = if seconds <= 0.0 then arr (const 0.0) else proc _ -> do
    f <- envLineSeg [800,100,100] [4.0, seconds] -< () -- frequency
    e <- envLineSeg [0, 1, 0, 0] [2.0, 2.0, seconds] -< () -- volume envelope
    s <- osc sineTable 0 -< f -- descending sine wave tone
    sRest <- delayLine 0.5 <<< shepard (seconds-0.5) -< () -- delayed other tones
    returnA -< (e * s * 0.1) + sRest
{-# SPECIALIZE INLINE shepard :: Time -> SigFun CCNF_D AudRate () Double #-}
{-# SPECIALIZE INLINE shepard :: Time -> SigFun SF AudRate () Double #-}
