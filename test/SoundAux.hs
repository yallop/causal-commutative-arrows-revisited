{-# LANGUAGE BangPatterns #-}
module SoundAux where

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import qualified Data.Vector.Unboxed as V

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

frac :: RealFrac r => r -> r
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
