{-# LANGUAGE GeneralizedNewtypeDeriving, EmptyDataDecls, FlexibleInstances, FlexibleContexts, BangPatterns, ExistentialQuantification, ScopedTypeVariables, Arrows #-}
import Prelude hiding (init,id,(.))
import Control.Category
import Control.Arrow
import Control.CCA.Instances
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import GHC.IO
import System.Random
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Codec.Wav
import Data.Audio
import Data.Int


-- data SF a b = SF { runSF :: a -> (b, SF a b) }
delay :: ArrowInit a => b -> a b b
delay = init

class Clock p where
    rate :: p -> Double  -- sampling rate

data AudRate
data CtrRate

instance Clock AudRate where
    rate _ = 44100

instance Clock CtrRate where
    rate _ = 4410

type AudSF a b  = SigFun AudRate a b
type CtrSF a b  = SigFun CtrRate a b

newtype Signal clk a b = Signal { strip :: CCNF_D a b } deriving (Category, Arrow, ArrowLoop, ArrowInit)
type SigFun clk a b = Signal clk a b

data Tab = Tab [Double] !Int !(UArray Int Double)

type Time            = Double
type TableSize       = Int
-- type PartialNum      = Double
type PartialStrength = Double
-- type PhaseOffset     = Double
-- type StartPt         = Double
-- type SegLength       = Double
-- type EndPt           = Double

outA :: Arrow a => a b b
outA = arr id

data Buf = Buf !Int !(Ptr Double)

updateBuf :: Buf -> Int -> Double -> IO Double
updateBuf (Buf _ a) i u = a `seq` i `seq` u `seq` do
    let p = a `advancePtr` i
    x' <- peek p
    poke p u
    return x'

peekBuf (Buf sz a) i = peek (a `advancePtr` (min (sz-1) i))

mkArr :: Int -> Buf
mkArr n = n `seq` Buf n (unsafePerformIO $
            Foreign.Marshal.newArray (replicate n 0))

sqrt2 :: Double
sqrt2 = sqrt 2

frac :: RealFrac r => r -> r
frac = snd . properFraction

aAt :: Tab -> Int -> Double
aAt (Tab _ sz a) i = unsafeAt a (min (sz-1) i)

seghlp :: forall p . Clock p =>
           [Double]  -- List of points to trace through.
        -> [Double]  -- List of durations for each line segment.
                     -- Needs to be one element fewer than 'iamps'.
        -> Signal p () (Double,Double,Double,Double)
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
        i <- delay 0 -< i'
        t <- delay 0 -< t'
      let a1 = aAt amps i
          a2 = aAt amps (i+1)
          d  = aAt durs i
      outA -< (a1,a2,t,d)

data Table = Table
    !Int                   -- size
    !(UArray Int Double)   -- table implementation
    !Bool                  -- Whether the table is normalized

instance Show Table where
    show (Table sz a n) = "Table with " ++ show sz ++ " entries: " ++ show a

funToTable :: (Double->Double) -> Bool -> Int -> Table
funToTable f normalize size =
    let delta = 1 / fromIntegral size
        ys = take size (map f [0, delta.. ]) ++ [head ys]
             -- make table one size larger as an extended guard point
        zs = if normalize then map (/ maxabs ys) ys else ys
        maxabs = maximum . map abs
    in Table size (listArray (0, size) zs) normalize

tableSinesF :: (Floating a, Enum a) => [a] -> a -> a
tableSinesF pss x = let phase = 2 * pi * x
               in sum (zipWith (*) [ sin (phase * pn) | pn <- [1..] ] pss)

tableSinesN :: TableSize -> [PartialStrength] -> Table
tableSinesN  size pss = tableSinesN_ pss True size
tableSines :: Int -> [Double] -> Table
tableSines   size pss = tableSinesN_ pss False size
tableSinesN_ :: [Double] -> Bool -> Int -> Table
tableSinesN_ pss = funToTable (tableSinesF pss)

sineTable :: Table
sineTable = tableSinesN 4096 [1]

envLine :: forall p . Clock p =>
        Double  -- Starting value.
     -> Double  -- Duration in seconds.
     -> Double  -- Value after 'dur' seconds.
     -> Signal p () Double
envLine a dur b =
    let sr = rate (undefined :: p)
    in proc () -> do
      rec
        y <- delay a -< y + (b-a) * (1 / sr / dur)
      outA -< y

envLineSeg :: Clock p =>
           [Double]  -- List of points to trace through.
        -> [Double]  -- List of durations for each line segment.
                     -- Needs to be one element fewer than 'amps'.
        -> Signal p () Double
envLineSeg amps durs =
    let sf = seghlp amps durs
    in proc () -> do
      (a1,a2,t,d) <- sf -< ()
      outA -< a1 + (a2-a1) * (t / d)

noiseWhite :: Int -> Signal p () Double
noiseWhite seed =
    let gen = mkStdGen seed
    in proc () -> do
      rec
        let (a,g') = random g :: (Double,StdGen)
        g <- delay gen -< g'
      outA -< a * 2 - 1

delayLine :: forall p . Clock p =>
         Double -> Signal p Double Double
delayLine maxdel =
    let sr = rate (undefined :: p)
        sz = truncate (sr * maxdel)
        buf = mkArr sz
    in proc x -> do
        rec
          let i' = if i == sz-1 then 0 else i+1
          i <- delay 0 -< i'
          y <- delay 0 -< x
        outA -< unsafePerformIO $ updateBuf buf i y

butter :: Clock p => Signal p (Double,ButterData) Double
butter = proc (sig, ButterData a1 a2 a3 a4 a5) -> do
    rec let t = sig - a4 * y' - a5 * y''
            y = t * a1 + a2 * y' + a3 * y''
        y'  <- delay 0 -< t
        y'' <- delay 0 -< y'
    outA -< y

filterLowPassBW :: forall p . Clock p => Signal p (Double, Double) Double
filterLowPassBW =
  let sr = rate (undefined :: p)
  in proc (sig, freq) -> do
       butter -< (sig, blpset freq sr)

blpset :: Double -> Double -> ButterData
blpset freq sr = ButterData a1 a2 a3 a4 a5
  where c = 1 / tan (pidsr * freq)
        csq = c * c; pidsr = pi / sr
        a1 = 1 / (1 + sqrt2 * c + csq)
        a2 = 2 * a1
        a3 = a1
        a4 = 2 * (1 - csq) * a1
        a5 = (1 - sqrt2 * c + csq) * a1

data ButterData = ButterData !Double !Double !Double !Double !Double

osc :: Clock p =>
         Table
      -> Double  -- Initial phase of sampling, expressed as a
                 -- fraction of a cycle (0 to 1).
      -> SigFun p Double Double
osc table iphs = osc_ iphs >>> readFromTableA table

readFromTable :: Table -> Double -> Double
readFromTable (Table sz array _) pos =
    let idx = truncate (fromIntegral sz * pos)  -- range must be [0,size]
    in array `unsafeAt` idx
{-# INLINE [0] readFromTable #-}

readFromTableA :: Arrow a => Table -> a Double Double
readFromTableA = arr . readFromTable

osc_ :: forall p a. Clock p =>
          Double -> SigFun p Double Double
osc_ phs =
    let sr = rate (undefined :: p)
    in proc freq -> do
      rec
        let delta = 1 / sr * freq
            phase = if next > 1 then frac next else next
        next <- delay phs -< frac (phase + delta)
      outA -< phase

flute ::  Time -> Double -> Double -> Double -> Double 
          -> AudSF () Double
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
    
tFlute = outFile "tFlute.wav" 5 $ flute 5 0.7 440 0.99 0.2 

main = tFlute

outFile = outFileHelp id

outFileHelp :: forall a p. (AudioSample a, Clock p) =>
            ([Double] -> [Double]) -- ^ Post-processing function.
         -> String              -- ^ Filename to write to.
         -> Double              -- ^ Duration of the wav in seconds.
         -> Signal p () a       -- ^ Signal representing the sound.
         -> IO ()
outFileHelp f filepath dur sf =
  let sr          = rate (undefined :: p)
      numChannels = numChans (undefined :: a)
      numSamples  = truncate (dur * sr) * numChannels
      dat         = map (fromSample . (*0.999))
                        (f (toSamples dur sf)) :: [Int32]
                    -- multiply by 0.999 to avoid wraparound at 1.0
      array       = listArray (0, numSamples-1) dat
      aud = Audio { sampleRate    = truncate sr,
                    channelNumber = numChannels,
                    sampleData    = array }
  in exportFile filepath aud

class AudioSample a where
    zero :: a
    mix :: a -> a -> a
    collapse :: a -> [Double]
    numChans :: a -> Int
      -- allows us to reify the number of channels from the type.

instance AudioSample Double where
    zero = 0
    mix = (+)
    collapse a = [a]
    numChans _ = 1

instance AudioSample (Double,Double) where
    zero = (0,0)
    mix (a,b) (c,d) = (a+c,b+d)
    collapse (a,b) = [a,b]
    numChans _ = 2

toSamples :: forall a p. (AudioSample a, Clock p) =>
             Double -> Signal p () a -> [Double]
toSamples dur sf =
  let sr          = rate     (undefined :: p)
      numChannels = numChans (undefined :: a)
      numSamples  = truncate (dur * sr) * numChannels
  in take numSamples $ concatMap collapse $ unfoldCCNF $ strip sf

unfoldCCNF :: CCNF_D () a -> [a]
unfoldCCNF = flip applyCCNF_D inp
  where inp = () : inp

{-

run :: SF a b -> [a] -> [b]
run _ [] = []
run (SF f) (x:xs) =
  let (y, f') = f x
  in y `seq` f' `seq` (y : run f' xs)

unfold :: SF () a -> [a]
unfold = flip run inp
  where inp = () : inp


instance Category SF where
  id = SF h where h x = (x, SF h)
  g . f = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in f' `seq` g' `seq` (z, SF (h f' g'))

instance Arrow SF where
  arr f = g
    where g = SF (\x -> (f x, g))
  first f = SF (g f)
    where
      g f (x, z) = f' `seq` ((y, z), SF (g f'))
        where (y, f') = runSF f x
  f &&& g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g x
        in ((y, z), SF (h f' g'))
  f *** g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f (fst x)
            (z, g') = runSF g (snd x)
        in ((y, z), SF (h f' g'))

instance ArrowLoop SF where
  loop sf = SF (g sf)
    where
      g f x = f' `seq` (y, SF (g f'))
        where ((y, z), f') = runSF f (x, z)

-- instance ArrowChoice SF where
--    left sf = SF (g sf)
--        where
--          g f x = case x of
--                    Left a -> let (y, f') = runSF f a in f' `seq` (Left y, SF (g f'))
--                    Right b -> (Right b, SF (g f))

instance ArrowCircuit SF where
  delay i = SF (f i)
    where f i x = (i, SF (f x))

-}


