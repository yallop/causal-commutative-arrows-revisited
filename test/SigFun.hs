module SigFun where

import Control.Category
import Control.Arrow
import Control.CCA.Types
import Control.CCA.Instances
import Data.IORef
import System.IO.Unsafe
import qualified Data.Vector.Unboxed.Mutable as MV

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
{-
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
-}

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

instance ArrowInitLine SF where
  initLine size i = SF (f newBuf)
    where
      newBuf = unsafePerformIO $ do
        b <- MV.new size 
        MV.set b i
        r <- newIORef 0
        return (b, r)
      f (b, r) x = unsafePerformIO $ do
         i <- readIORef r
         x' <- MV.unsafeRead b i
         MV.unsafeWrite b i x
         let i' = i + 1 
         writeIORef r $ if i' >= size then 0 else i'
         return (x', SF (f (b, r)))

