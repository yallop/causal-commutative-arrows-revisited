{-# LANGUAGE TupleSections, RecursiveDo, GADTs, TypeOperators, KindSignatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Control.CCA.Instances where

import Control.Category
import Control.Applicative ((<*>), (<$>))
import Control.Arrow
import Control.CCA.Types 
import Control.Monad.ST
import Control.Monad.ST.Unsafe as STU
import Data.IORef
import Data.STRef
import System.IO.Unsafe
import qualified Data.Vector.Unboxed.Mutable as MV

import Prelude hiding ((.), init, id)

-- loopB-based CCA normal form
data CCNF_B :: * -> * -> * where
   ArrB   :: (a -> b) -> CCNF_B a b
   LoopB :: ((a, (c, d)) -> (b, (c, d))) -> d -> CCNF_B a b

-- CCNF_B is an instance of ArrowInit (and hence of the superclasses
-- ArrowLoop, Arrow and Category)
instance Category CCNF_B where
   id = ArrB id

   ArrB g    . ArrB f    = ArrB (g . f)
   ArrB g    . LoopB f i = LoopB (\a -> let (x,y) = f a in (g x, y)) i
   LoopB f i . ArrB h    = LoopB (\ ~(x,y) -> f (h x, y)) i
   LoopB f i . LoopB g j = LoopB
      ((\ ~(a, ((b, d), (c, e))) ->
         let (a', b') = g (a, (d, e))
             ((e'',(a'',b'')),(c'',d'')) = (f (a', (b, c)), b')
         in (e'',((a'',c''),(b'',d'')))))
      (i,j)

instance Arrow CCNF_B where
   arr = \f -> ArrB (arr f)

   first (ArrB f) = ArrB (\ ~(x,y) -> (f x, y))
   first (LoopB f i) = LoopB (\ ~((a, b), c) ->
                               let (a', b') = f (a, c)
                               in ((a', b), b')) i

instance ArrowLoop CCNF_B where
   loop (ArrB f)  = LoopB (\ ~(a,(b,c)) ->
                            let ((a',b'),c') = (f (a,b),c)
                            in (a',(b',c'))) ()
   loop (LoopB f i) = LoopB (arr
                             (\ ~(a, ((b, d), (c, e))) ->
                               let (((a', b'), (d', e'))) = (f ((a, b), (d, e)))
                               in (a', ((b', d'), (c, e')))))
                      ((), i)

instance ArrowInit CCNF_B where
   init = \i -> LoopB ((\ ~(b,(a,c)) -> (c,(a,b)))) i

-- evaluate a term in CCNF_B normal form at an ArrowInit instance
observeB :: (ArrowInit arr) => CCNF_B a b -> (a `arr` b)
observeB (ArrB f) = arr f
observeB (LoopB f i) = loop (arr f >>> second (second (init i)))

-- loopD-based CCA normal form
data CCNF_D :: * -> * -> * where
   ArrD   :: (a -> b) -> CCNF_D a b
   LoopD :: e -> ((b,e) -> (c,e)) -> CCNF_D b c

-- evaluate a term in CCNF_D normal form at an ArrowInit instance
observeD :: ArrowInit arr => CCNF_D a b -> (a `arr` b)
observeD (ArrD f) = arr f
observeD (LoopD i f) = loop (arr f >>> second (init i))

-- apply a normalized (CCNF_D) computation to transform a stream
applyCCNF_D :: CCNF_D a b -> [a] -> [b]
applyCCNF_D (ArrD f) = map f
applyCCNF_D (LoopD i f) = runCCNF i f
  where
   -- from Section 6 of the ICFP paper
   runCCNF :: e -> ((b,e) -> (c,e)) -> [b] -> [c]
   runCCNF i f = g i
     where g i (x:xs) = let (y, i') = f (x, i)
                        in y : g i' xs

-- CCNF_D is an instance of ArrowInit (and hence of the superclasses
-- ArrowLoop, Arrow and Category)
-- (adapted from Paul Liu's dissertation)
instance Category CCNF_D where
   id = ArrD id

   ArrD g    . ArrD f    = ArrD (g . f)
   LoopD i g . ArrD f    = LoopD i (g . first f)
   ArrD g    . LoopD i f = LoopD i (first g . f)
   LoopD j g . LoopD i f = LoopD (i, j) (assoc' (juggle' (first g) . first f))
   

instance Arrow CCNF_D where
   arr = ArrD
   
   first (ArrD f) = ArrD (first f)
   first (LoopD i f) = LoopD i (juggle' (first f))
   
   second f = arr swap >>> first f >>> arr swap
   
   f *** g = first f >>> second g
   
   f &&& g = arr (\x -> (x, x)) >>> f *** g
   

instance ArrowLoop CCNF_D where
   loop (ArrD f ) = ArrD (trace f)
   loop (LoopD i f) = LoopD i (trace (juggle' f))
   

instance ArrowInit CCNF_D where
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

-- loopD-based CCA normal form, with transition function uncurried

data CCNF_D' :: * -> * -> * where
   ArrD'   :: (a -> b) -> CCNF_D' a b
   LoopD' :: e -> ((b,e) -> (c,e)) -> CCNF_D' b c

-- evaluate a term in CCNF_D' normal form at an ArrowInit instance
observeD' :: ArrowInit arr => CCNF_D' a b -> (a `arr` b)
observeD' (ArrD' f) = arr f
observeD' (LoopD' i f) = loop (arr f >>> second (init i))

-- apply a normalized (CCNF_D') computation to transform a stream
applyCCNF_D' :: CCNF_D' a b -> [a] -> [b]
applyCCNF_D' (ArrD' f) = map f
applyCCNF_D' (LoopD' i f) = runCCNF i f
  where
   -- from Section 6 of the ICFP paper
   runCCNF :: e -> ((b,e) -> (c,e)) -> [b] -> [c]
   runCCNF i f = g i
     where g i (x:xs) = let (y, i') = f (x, i)
                        in y : g i' xs

-- CCNF_D' is an instance of ArrowInit (and hence of the superclasses
-- ArrowLoop, Arrow and Category)
-- (adapted from Paul Liu's dissertation)
instance Category CCNF_D' where
   id = ArrD' id

   ArrD' g    . ArrD' f    = ArrD' (g . f)
   LoopD' i g . ArrD' f    = LoopD' i (g . first f)
   ArrD' g    . LoopD' i f = LoopD' i (first g . f)
   LoopD' j g . LoopD' i f = LoopD' (i, j) (assoc' (juggle' (first g) . first f))
   

instance Arrow CCNF_D' where
   arr = ArrD'
   
   first (ArrD' f) = ArrD' (first f)
   first (LoopD' i f) = LoopD' i (juggle' (first f))
   
   second f = arr swap >>> first f >>> arr swap
   
   f *** g = first f >>> second g
   
   f &&& g = arr (\x -> (x, x)) >>> f *** g
   

instance ArrowLoop CCNF_D' where
   loop (ArrD' f ) = ArrD' (trace f)
   loop (LoopD' i f) = LoopD' i (trace (juggle' f))
   

instance ArrowInit CCNF_D' where
   init i = LoopD' i swap
 
-- ST Monad based CCNF

data CCNF_ST s a b where
   ArrST   :: (a -> b) -> CCNF_ST s a b
   LoopST :: ST s e -> (e -> b -> ST s c) -> CCNF_ST s b c

instance Category (CCNF_ST s) where
   id = ArrST id
   ArrST g    . ArrST f      = ArrST (g . f)
   LoopST i g . ArrST f     = LoopST i (\i -> g i . f)
   ArrST g    . LoopST i f  = LoopST i (\i -> fmap g . f i)
   LoopST j g . LoopST i f = LoopST ((,) <$> i <*> j) h
    where
      h (i, j) x = do
        y <- f i x
        z <- g j y
        return z

instance Arrow (CCNF_ST s) where
   arr = ArrST
   first (ArrST f)     = ArrST (first f)
   first (LoopST i f) = LoopST i aux
     where
       aux i ~(x, y) = (,y) <$> f i x
   second (ArrST f) = ArrST (second f)
   second (LoopST i f) = LoopST i aux
     where
       aux i ~(x, y) = (x,) <$> f i y
   ArrST f *** ArrST g = ArrST (f *** g)
   ArrST f *** LoopST j g = LoopST j aux
     where
       aux j ~(x, y) = do
        let u = f x
        v <- g j y
        return (u, v)
   LoopST i f *** ArrST g = LoopST i aux
     where
       aux i ~(x, y) = do
        u <- f i x
        let v = g y
        return (u, v)
   LoopST i f *** LoopST j g = LoopST ((,) <$> i <*> j) aux
     where
       aux ~(i, j) ~(x, y) = do
        u <- f i x
        v <- g j y
        return (u, v)
   ArrST f &&& ArrST g = ArrST (f &&& g)
   ArrST f &&& LoopST j g = LoopST j aux
     where
       aux j x = do
        let u = f x
        v <- g j x 
        return (u, v)
   LoopST i f &&& ArrST g = LoopST i aux
     where
       aux i x = do
        u <- f i x
        let v = g x 
        return (u, v)
   LoopST i f &&& LoopST j g = LoopST ((,) <$> i <*> j) aux
     where
       aux (i, j) x = do
        u <- f i x
        v <- g j x 
        return (u, v)
--   second f = arr swap >>> first f >>> arr swap
--   f *** g = first f >>> second g
--   f &&& g = arr (\x -> (x, x)) >>> f *** g
   

 
instance ArrowLoop (CCNF_ST s) where
   loop (ArrST f )   = ArrST (trace f)
   loop (LoopST i f) = LoopST i h
     where
       -- f :: ((a, c), e) -> ST s (b, c)
       -- h :: (a, e) -> ST s b
       h i x = do
         rec (y, j) <- f i (x, j)
         return y

instance ArrowInit (CCNF_ST s) where
   init i = LoopST (newSTRef i) f
     where
       f i x = do
         y <- readSTRef i
         writeSTRef i x
         return y

class ArrowInit a => ArrowInitLine a where
   initLine :: MV.Unbox b => Int -> b -> a b b

instance ArrowInitLine (CCNF_ST s) where
   initLine size i = LoopST newBuf f
     where
       newBuf = do
         b <- MV.new size
         MV.set b i 
         r <- newSTRef 0
         return (b, r)
       f (b, r) x = do
         i <- readSTRef r
         x' <- MV.unsafeRead b i
         MV.unsafeWrite b i x
         let i' = i + 1 
         writeSTRef r $ if i' >= size then 0 else i'
         return x'

instance ArrowInitLine CCNF_D where
   initLine size i = LoopD newBuf f
     where
       newBuf = unsafePerformIO $ do
         b <- MV.new size 
         MV.set b i
         r <- newIORef 0
         return (b, r)
       f (x, s@(b, r)) = unsafePerformIO $ do
         i <- readIORef r
         x' <- MV.unsafeRead b i
         MV.unsafeWrite b i x
         let i' = i + 1 
         writeIORef r $ if i' >= size then 0 else i'
         return (x', s)


