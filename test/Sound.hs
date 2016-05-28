{-# LANGUAGE Arrows #-}
module Sound where

import Prelude hiding (init,id,(.))
import Control.Category
import Control.Arrow
import Control.CCA.Types
import Control.CCA.Instances
--import System.Random
import System.Random.Mersenne.Pure64
import System.IO.Unsafe
import SoundAux
import SoundFun
import qualified Data.Vector.Unboxed as V

outA :: Arrow a => a b b
outA = arr id

seghlp :: ArrowInit a =>
           [Double]  -- List of points to trace through.
        -> [Double]  -- List of durations for each line segment.
                     -- Needs to be one element fewer than 'iamps'.
        -> a () (Double,Double,Double,Double)
seghlp iamps idurs =
{-
    let sr = audioRate 
        sz = V.length amps
        amps = mkTab iamps
        durs = mkTab $ map (*sr) idurs
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
-}
    loop (arr (\((), (i, t)) ->
            let amps = mkTab iamps
                durs = mkTab $ map (*sr) idurs
                (t', i') = if t >= durs `aAt` i
                         then if i == sz-2 then (t+1, i) else (0, i+1)
                           else (t+1, i)
                a1 = aAt amps i
                a2 = aAt amps (i+1)
                d  = aAt durs i
            in ((a1,a2,t,d), (i', t'))) >>> second (init (0, 0)))
    where
      sr = audioRate 
      sz = length iamps

            
        

envLine :: ArrowInit a =>
        Double  -- Starting value.
     -> Double  -- Duration in seconds.
     -> Double  -- Value after 'dur' seconds.
     -> a () Double
envLine a dur b =
    let sr = audioRate 
    in proc () -> do
      rec
        y <- init  a -< y + (b-a) * (1 / sr / dur)
      outA -< y


envLineSeg :: ArrowInit a =>
           [Double]  -- List of points to trace through.
        -> [Double]  -- List of durations for each line segment.
                     -- Needs to be one element fewer than 'amps'.
        -> a () Double
envLineSeg amps durs =
    let sf = seghlp amps durs
    in proc () -> do
      (a1,a2,t,d) <- sf -< ()
      outA -< a1 + (a2-a1) * (t / d)


noiseWhite :: ArrowInit a => Int -> a () Double
noiseWhite seed =
    let gen = pureMT $ fromIntegral seed 
    in proc () -> do
      rec
        let (a,g') = randomDouble g 
        g <- init gen -< g'
      outA -< a * 2 - 1


delayLine :: ArrowInit a =>
         Double -> a Double Double
delayLine maxdel =
    let sr = audioRate 
        sz = truncate (sr * maxdel)
        buf = mkBuf sz
    in proc x -> do
        rec
          let i' = if i == sz-1 then 0 else i+1
          i <- init 0 -< i'
          y <- init 0 -< x
        outA -< unsafePerformIO $ updateBuf buf i y


butter :: ArrowInit a => a (Double,ButterData) Double
butter = proc (sig, ButterData a1 a2 a3 a4 a5) -> do
    rec let t = sig - a4 * y' - a5 * y''
            y = t * a1 + a2 * y' + a3 * y''
        y'  <- init 0 -< t
        y'' <- init 0 -< y'
    outA -< y


filterLowPassBW :: ArrowInit a => a (Double, Double) Double
filterLowPassBW =
  let sr = audioRate 
  in proc (sig, freq) -> do
       butter -< (sig, blpset freq sr)


osc :: ArrowInit a =>
         Table
      -> Double  -- Initial phase of sampling, expressed as a
                 -- fraction of a cycle (0 to 1).
      -> a Double Double
osc table iphs = osc_ iphs >>> readFromTableA table


readFromTableA :: Arrow a => Table -> a Double Double
readFromTableA = arr . readFromTable


osc_ :: ArrowInit a =>
          Double -> a Double Double
osc_ phs =
    let sr = audioRate 
    in proc freq -> do
      rec
        let delta = 1 / sr * freq
            phase = if next > 1 then frac next else next
        next <- init phs -< frac (phase + delta)
      outA -< phase

flute :: ArrowInit a => Time -> Double -> Double -> Double -> Double -> a () Double 
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
{-# SPECIALIZE INLINE flute :: Time -> Double -> Double -> Double -> Double -> CCNF_D () Double #-}
{-# SPECIALIZE INLINE flute :: Time -> Double -> Double -> Double -> Double -> SF () Double #-}

shepard :: ArrowInit a => Time -> a () Double 
shepard seconds = if seconds <= 0.0 then arr (const 0.0) else proc _ -> do
    f <- envLineSeg [800,100,100] [4.0, seconds] -< () -- frequency
    e <- envLineSeg [0, 1, 0, 0] [2.0, 2.0, seconds] -< () -- volume envelope
    s <- osc sineTable 0 -< f -- descending sine wave tone
    sRest <- delayLine 0.5 <<< shepard (seconds-0.5) -< () -- delayed other tones
    returnA -< (e * s * 0.1) + sRest
{-# SPECIALIZE INLINE shepard :: Time -> CCNF_D () Double #-}
{-# SPECIALIZE INLINE shepard :: Time -> SF () Double #-}
