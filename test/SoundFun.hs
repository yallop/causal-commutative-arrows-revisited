{-# LANGUAGE EmptyDataDecls #-}
module SoundFun where

import Control.Category
import Control.Arrow
import Control.CCA.Types
import Prelude hiding (init,id,(.))

-- generic Clock rate
class Clock p where
    rate :: p -> Double  -- sampling rate

data AudRate
data CtrRate

instance Clock AudRate where
    rate _ = 44100

instance Clock CtrRate where
    rate _ = 4410

-- a singal function that is generic in arrow and clock rate 
newtype SigFun arr clk a b = SigFun { strip :: arr a b } -- deriving (Category, Arrow, ArrowLoop, ArrowInit)

instance Category arr => Category (SigFun arr clk) where
  id = SigFun id
  SigFun x . SigFun y = SigFun (x . y)

instance Arrow arr => Arrow (SigFun arr clk) where
  arr f = SigFun (arr f)
  first (SigFun f) = SigFun (first f)
  second (SigFun f) = SigFun (second f)
  SigFun f *** SigFun g = SigFun (f *** g)
  SigFun f &&& SigFun g = SigFun (f &&& g)
  
instance ArrowLoop arr => ArrowLoop (SigFun arr clk) where
  loop (SigFun f) = SigFun (loop f) 

instance ArrowInit arr => ArrowInit (SigFun arr clk) where
  init x = SigFun (init x)

-- The continuation based SF implementation
newtype SF a b = SF { runSF :: (a -> (b, SF a b)) }

instance Category SF where
  id = SF h where h x = (x, SF h)
  g . f = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in (z, SF (h f' g'))

instance Arrow SF where
  arr f = g
    where g = SF (\x -> (f x, g))
  first f = SF (g f)
    where
      g f (x, z) = ((y, z), SF (g f'))
        where (y, f') = runSF f x
  second f = SF (g f)
    where
      g f (z, x) = ((z, y), SF (g f'))
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
      g f x = (y, SF (g f'))
        where ((y, z), f') = runSF f (x, z)

instance ArrowInit SF where
  init i = SF (f i)
    where f i x = (i, SF (f x))
  loopD i g = SF (f i) 
    where
      f i x = 
        let (y, i') = g (x, i)
        in (y, SF (f i'))
