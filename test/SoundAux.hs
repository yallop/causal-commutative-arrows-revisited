{-# LANGUAGE GeneralizedNewtypeDeriving, EmptyDataDecls, FlexibleInstances, FlexibleContexts, BangPatterns, ExistentialQuantification, ScopedTypeVariables, Arrows #-}
module SoundAux where

import Control.Arrow
import Prelude hiding (init,id)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import GHC.IO
import System.Environment
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Codec.Wav
import Data.Audio
import Data.Int

data Tab = Tab [Double] !Int !(UArray Int Double)

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

data ButterData = ButterData !Double !Double !Double !Double !Double

readFromTable :: Table -> Double -> Double
readFromTable (Table sz array _) pos =
    let idx = truncate (fromIntegral sz * pos)  -- range must be [0,size]
    in array `unsafeAt` idx
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


