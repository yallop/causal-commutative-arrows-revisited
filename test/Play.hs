{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.CCA
import Control.CCA.CCNF
import Control.CCA.Types
import Control.CCA.Instances
import SoundAux
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import GHC.IO
import System.Environment
import System.Random
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Codec.Wav
import Data.Audio
import Data.Int
import Data.List
import SoundFun
import qualified SoundTH
import qualified Sound

outFile filepath dur stream =
  let sr          = 44100
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

unfoldSF :: SF () a -> [a]
unfoldSF (SF f) = let (x, f') = f () in x : unfoldSF f'

unfoldTH :: (b, ((), b) -> (a, b)) -> [a]
unfoldTH (i, f) = next i
  where
    next i = let (x, i') = f ((), i)
             in x : next i' 

unfoldCCNF_D :: CCNF_D () a -> [a]
unfoldCCNF_D (ArrD f) = map f inp
  where inp = () : inp
unfoldCCNF_D (LoopD i f) = unfoldTH (i, f)

unfoldNF = unfoldCCNF_D 

playlist = 
  [ ("-th",        -- Template Haskell optimized to CCNF
        [ ("flute",   (5.0, unfoldTH fluteTH))
        , ("shepard", (16.0, unfoldTH shepardTH))
        ])
  , ("-nf",        -- CCNF based arrows
        [ ("flute",   (5.0, unfoldNF fluteNF))
        , ("shepard", (16.0, unfoldNF shepardNF))
        ])
  , ("-sf",        -- SF based arrows
        [ ("flute",   (5.0, unfoldSF fluteSF))
        , ("shepard", (16.0, unfoldSF shepardSF))
        ])
  ]
main = do
  cmd <- getProgName
  let usage = error $ "USAGE: " ++ cmd ++ " " ++ bracket opts ++ " " ++ bracket names 
  arg <- getArgs
  case arg of
    [ opt, name ] -> 
      case lookup opt playlist >>= lookup name of
        Just (dur, stream) -> outFile name dur stream
        _ -> usage
    [ name ] -> case lookup "template" playlist >>= lookup name of
        Just (dur, stream) -> outFile name dur stream
        _ -> usage
    _ -> usage
  where
    opts = intercalate "|" $ map fst playlist
    names = intercalate "|" $ map fst $ snd $ head playlist
    bracket s = "[" ++ s ++ "]"

safeHead [] = ""
safeHead (x:xs) = x

-- fluteTH' :: SF () Double
-- fluteTH' = $(norm fluteA)

fluteTH = $(normOpt SoundTH.fluteA)

-- shepardTH' :: SF () Double
-- shepardTH' = $(norm shepardA) 

shepardTH = $(normOpt SoundTH.shepardA) 

fluteNF :: CCNF_D () Double
fluteNF = strip $ Sound.flute 5 0.3 440 0.99 0.2 

shepardNF :: CCNF_D () Double
shepardNF = strip $ Sound.shepard 16.0 

fluteSF :: SF () Double
fluteSF = strip $ Sound.flute 5 0.3 440 0.99 0.2 

shepardSF :: SF () Double
shepardSF = strip $ Sound.shepard 16.0 


