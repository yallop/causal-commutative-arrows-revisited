{-# LANGUAGE CPP, Arrows, GADTs, NoImplicitPrelude #-}
import Prelude hiding (id, (.), exp, init)
import Control.Category
import Control.Arrow
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import System.Random.Mersenne.Pure64
import qualified Data.Vector.Unboxed as V
--import Criterion.Main
import Data.Time
import Codec.Wav
import Data.Int
import Data.Audio
import Data.Array.Unboxed

{-
class Arrow a where
   arr :: (b -> c) -> a b c
   (>>>) :: a b c -> a c d -> a b d
   first :: a b c -> a (b, d) (c, d)

class Arrow a => ArrowLoop a where
   loop :: a (b, d) (c, d) -> a b c

instance Arrow (->) where
   arr = \x -> x 
   f >>> g = g . f
   first f = \ ~(x, y) -> (f x, y)

second f = arr swap >>> first f >>> arr swap
f *** g = first f >>> second g
f &&& g = arr (\x -> (x, x)) >>> f *** g

-}

class ArrowLoop a => ArrowInit a where
   init :: b -> a b b

data CCNF a b where
   Arr   :: (a -> b) -> CCNF a b
   LoopD :: e -> ((b,e) -> (c,e)) -> CCNF b c

instance Category CCNF where
   id = Arr id
   Arr g    . Arr f      = Arr (g . f)
   LoopD i g . Arr f     = LoopD i (g . first f)
   Arr g    . LoopD i f  = LoopD i (first g . f)
   LoopD j g . LoopD i f = LoopD (i, j) (assoc' (juggle' (first g) . first f))

instance Arrow CCNF where
   arr = Arr
   first (Arr f)     = Arr (first f)
   first (LoopD i f) = LoopD i (juggle' (first f))
   
instance ArrowLoop CCNF where
   loop (Arr f )    = Arr (trace f)
   loop (LoopD i f) = LoopD i (trace (juggle' f))

instance ArrowInit CCNF where
   init i = LoopD i swap

-- auxiliary definitions
trace :: ((b, d) -> (c, d)) -> b -> c
trace = \f b -> let (c, d) = f (b, d) in c
{-# INLINE trace #-}

swap ::   (a,b) -> (b,a)
swap = \ ~(a,b) -> (b,a)
{-# INLINE swap #-}

dup :: a -> (a,a)
dup = \a -> (a,a)
{-# INLINE dup #-}

assoc :: ((a, b), c) -> (a, (b, c))
assoc    (~(a, b), c) =  (a, (b, c))
{-# INLINE assoc #-}

cossa :: (a, (b, c)) -> ((a, b), c)
cossa    (a, ~(b, c)) =  ((a, b), c)
{-# INLINE cossa #-}

assoc' :: (((a, b), c) -> ((d, e), f)) ->
          ((a, (b, c)) -> (d, (e, f)))
assoc' f = assoc . f . cossa
{-# INLINE assoc' #-}

juggle :: ((a, b), c) -> ((a, c), b)
juggle    (~(a, b), c) =  ((a, c), b)
{-# INLINE juggle #-}

juggle':: (((a, c), b) -> ((d, e), f)) ->
          (((a, b), c) -> ((d, f), e))
juggle' f = juggle . f . juggle
{-# INLINE juggle' #-} 

integral :: ArrowInit arr => arr Double Double
integral = loop (arr (\ (v, i) -> i + dt * v) >>>
                 init 0 >>> arr dup)

sr = 44100 :: Int
dt = 1 / (fromIntegral sr) :: Double

fixA :: ArrowLoop arr => arr a a -> arr b a
fixA = \f -> loop (second f >>> arr snd >>> arr dup)

exp :: ArrowInit a => a () Double
exp = fixA (integral >>> arr (+1))

sine :: ArrowInit a => Double -> a () Double
sine freq = proc _ -> do
  rec x <- init i -< r
      y <- init 0 -< x
      let r = c * x - y
  arr id -< r
  where
    omh = 2 * pi / (fromIntegral sr) * freq
    i = sin omh
    c = 2 * cos omh

{-
sine :: ArrowInit a => Double -> a () Double
sine freq =
  let omh = 2*pi/(fromIntegral sr)*freq
      c = 2 * cos omh
      i = sin omh
   in fixA (init i >>> arr dup >>>
             ((init 0 >>> arr negate) *** arr (c*)) >>>
             arr (uncurry (+)))
-}

sineP :: ArrowInit a => Double -> a () Double
sineP freq
  = (loop
       (arr (\ (_, r) -> r) >>>
          (init i >>> arr (\ x -> (x, x)))
            >>>
            (first (init 0) >>>
               arr (\ (y, x) ->
                    let r = c * x - y
                      in (r, r)))))
  where omh = 2 * pi / (fromIntegral sr) * freq
        i = sin omh
        c = 2 * cos omh

nth :: Int -> CCNF () a -> a
nth n (Arr f) = f ()
nth n (LoopD i f) = aux n i
  where
    aux n i = x `seq` if n == 0 then x else aux (n-1) i'
      where (x, i') = f ((), i)
{-# INLINE nth #-}

unfoldTH :: (b, ((), b) -> (a, b)) -> [a]
unfoldTH (i, f) = next i
  where
    next i = let (x, i') = f ((), i)
             in x : next i'
{-# INLINE unfoldTH #-}

unfoldNF :: CCNF () a -> [a]
unfoldNF (Arr f) = map f inp
  where inp = () : inp
unfoldNF (LoopD i f) = unfoldTH (i, f)
{-# INLINE unfoldNF #-}

n = 1000000
-- sine_t = sine 47
-- sineP_t = sineP 47

-- main = print e >> print s

audioRate = 44100 :: Double

type Tab = V.Vector Double

type Time            = Double
type TableSize       = Int
-- type PartialNum      = Double
type PartialStrength = Double
-- type PhaseOffset     = Double
-- type StartPt         = Double
-- type SegLength       = Double
-- type EndPt           = Double

data Buf = Buf !Int !(Ptr Double)

updateBuf :: Buf -> Int -> Double -> IO Double
updateBuf (Buf _ a) i u = a `seq` i `seq` u `seq` do
    let p = a `advancePtr` i
    x' <- peek p
    poke p u
    return x'

mkBuf :: Int -> Buf
mkBuf n = n `seq` Buf n (unsafePerformIO $
            Foreign.Marshal.newArray (replicate n 0))

sqrt2 :: Double
sqrt2 = sqrt 2

frac :: Double -> Double
frac = snd . properFraction

aAt :: Tab -> Int -> Double
aAt arr i = V.unsafeIndex arr $ min (V.length arr - 1) i

mkTab :: [Double] -> Tab
mkTab = V.fromList

type Table = V.Vector Double

funToTable :: (Double->Double) -> Bool -> Int -> Table
funToTable f normalize size =
    let arr = V.generate size (\i -> f $ fromIntegral i / fromIntegral size)
    in if normalize then V.map (/ (V.maximum $ V.map abs arr)) arr else arr

tableSinesF :: [Double] -> Double -> Double
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

data ButterData = ButterData !Double !Double !Double !Double !Double

readFromTable :: Table -> Double -> Double
readFromTable arr pos =
    let size = V.length arr
        idx  = truncate (fromIntegral size * pos)  -- range must be [0,size]
    in if idx < 0 then error (show (pos, size, idx)) else (V.!) arr idx
{-# INLINE [0] readFromTable #-}

blpset :: Double -> Double -> ButterData
blpset freq sr = ButterData a1 a2 a3 a4 a5
  where c = 1 / tan (pidsr * freq)
        csq = c * c; pidsr = pi / sr
        a1 = 1 / (1 + sqrt2 * c + csq)
        a2 = 2 * a1
        a3 = a1
        a4 = 2 * (1 - csq) * a1
        a5 = (1 - sqrt2 * c + csq) * a1

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
{-# SPECIALIZE INLINE seghlp :: [Double] -> [Double] -> CCNF () (Double, Double, Double, Double) #-}

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
{-# SPECIALIZE INLINE envLine :: Double -> Double -> Double -> CCNF () Double #-}


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
{-# SPECIALIZE INLINE envLineSeg :: [Double] -> [Double] -> CCNF () Double #-}

noiseWhite :: ArrowInit a => Int -> a () Double
noiseWhite seed =
    let gen = pureMT $ fromIntegral seed 
    in proc () -> do
      rec
        let (a,g') = randomDouble g 
        g <- init gen -< g'
      outA -< a * 2 - 1
{-# SPECIALIZE INLINE noiseWhite :: Int -> CCNF () Double #-}


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
{-# SPECIALIZE INLINE delayLine :: Double -> CCNF Double Double #-}


butter :: ArrowInit a => a (Double,ButterData) Double
butter = proc (sig, ButterData a1 a2 a3 a4 a5) -> do
    rec let t = sig - a4 * y' - a5 * y''
            y = t * a1 + a2 * y' + a3 * y''
        y'  <- init 0 -< t
        y'' <- init 0 -< y'
    outA -< y
{-# SPECIALIZE INLINE butter :: CCNF (Double, ButterData) Double #-}


filterLowPassBW :: ArrowInit a => a (Double, Double) Double
filterLowPassBW =
  let sr = audioRate 
  in proc (sig, freq) -> do
       butter -< (sig, blpset freq sr)
{-# SPECIALIZE INLINE filterLowPassBW :: CCNF (Double, Double) Double #-}


osc :: ArrowInit a =>
         Table
      -> Double  -- Initial phase of sampling, expressed as a
                 -- fraction of a cycle (0 to 1).
      -> a Double Double
osc table iphs = osc_ iphs >>> readFromTableA table
{-# SPECIALIZE INLINE osc :: Table -> Double -> CCNF Double Double #-}


readFromTableA :: Arrow a => Table -> a Double Double
readFromTableA = arr . readFromTable
{-# SPECIALIZE INLINE readFromTableA :: Table -> CCNF Double Double #-}


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
{-# SPECIALIZE INLINE osc_ :: Double -> CCNF Double Double #-}

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
{-# SPECIALIZE INLINE flute :: Time -> Double -> Double -> Double -> Double -> CCNF () Double #-}

shepard :: ArrowInit a => Time -> a () Double 
shepard seconds = if seconds <= 0.0 then arr (const 0.0) else proc _ -> do
    f <- envLineSeg [800,100,100] [4.0, seconds] -< () -- frequency
    e <- envLineSeg [0, 1, 0, 0] [2.0, 2.0, seconds] -< () -- volume envelope
    s <- osc sineTable 0 -< f -- descending sine wave tone
    sRest <- delayLine 0.5 <<< shepard (seconds-0.5) -< () -- delayed other tones
    returnA -< (e * s * 0.1) + sRest
{-# SPECIALIZE INLINE shepard :: Time -> CCNF () Double #-}


fluteA, shepardA :: CCNF () Double
fluteA = flute 5 0.3 440 0.99 0.2
shepardA = shepard 5.0
{-# INLINE fluteA #-}
{-# INLINE shepardA #-}

{-
main = defaultMain 
  [ bgroup "ccnf" [ bench "flute"   $ nf (flip nth fluteA)          (5 * floor audioRate)
                  , bench "shepard" $ nf (flip nth shepardA)        (16 * floor audioRate) 
                  ]
  ]

main = defaultMain 
  [ bgroup "ccnf" [ bench "exp"   $ nf (flip nth exp)        n -- the first one always take longer!
                  , bench "exp"   $ nf (flip nth exp)        n
                  , bench "sine"  $ nf (flip nth $ sine 47)  n
                  , bench "sineP" $ nf (flip nth $ sineP 47) n]
  ] 
-}


sine_t = sine 47
sineP_t = sineP 47

main = do
  timeIt "sine"  $ print (nth n sine_t) 
  timeIt "sintP" $ print (nth n sineP_t)
  timeIt "flute" $ print (nth (5 * floor audioRate) fluteA)
  timeIt "shepard" $ print (nth (5 * floor audioRate) shepardA)
  timeIt "flute.wav" $ outFile "flute.wav" 5 (unfoldNF fluteA)
  timeIt "shepard.wav" $ outFile "shepard.wav" 5 (unfoldNF shepardA)


timeIt s m = do
  t0 <- getCurrentTime
  m
  t1 <- getCurrentTime
  putStrLn $ s ++ ": " ++ show (diffUTCTime t1 t0)
  
outFile filepath dur stream =
  let sr          = audioRate
      numChannels = 1
      numSamples  = truncate (dur * sr) * numChannels
      dat         = map (fromSample . (*0.999))
                        (take numSamples stream) :: [Int32]
                    -- multiply by 0.999 to avoid wraparound at 1.0
      array       = listArray (0, numSamples-1) dat
      aud = Audio { sampleRate    = truncate sr,
                    channelNumber = numChannels,
                    sampleData    = array }
  in exportFile filepath aud

