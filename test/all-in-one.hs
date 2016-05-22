{-# LANGUAGE Arrows, GADTs, NoImplicitPrelude #-}
import Prelude hiding (exp, init)
import Criterion.Main

class Arrow a where
   arr :: (b -> c) -> a b c
   (>>>) :: a b c -> a c d -> a b d
   first :: a b c -> a (b, d) (c, d)

class Arrow a => ArrowLoop a where
   loop :: a (b, d) (c, d) -> a b c

class ArrowLoop a => ArrowInit a where
   init :: b -> a b b

data CCNF a b where
   Arr   :: (a -> b) -> CCNF a b
   LoopD :: e -> ((b,e) -> (c,e)) -> CCNF b c

instance Arrow (->) where
   arr = \x -> x 
   f >>> g = g . f
   first f = \ ~(x, y) -> (f x, y)

instance Arrow CCNF where
   arr = Arr
   Arr f     >>> Arr g     = Arr (f >>> g)
   Arr f     >>> LoopD i g = LoopD i (first f >>> g)
   LoopD i f >>> Arr g     = LoopD i (f >>> first g)
   LoopD i f >>> LoopD j g = LoopD (i, j) (assoc' (juggle' (first g) . first f))
    
   first (Arr f)     = Arr (first f)
   first (LoopD i f) = LoopD i (juggle' (first f))
   
second f = arr swap >>> first f >>> arr swap
f *** g = first f >>> second g
f &&& g = arr (\x -> (x, x)) >>> f *** g

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

{-
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
-}

sine :: ArrowInit a => Double -> a () Double
sine freq =
  let omh = 2*pi/(fromIntegral sr)*freq
      c = 2 * cos omh
      i = sin omh
   in fixA (init i >>> arr dup >>>
             ((init 0 >>> arr negate) *** arr (c*)) >>>
             arr (uncurry (+)))

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

n = 1000000
-- sine_t = sine 47
-- sineP_t = sineP 47

-- main = print e >> print s

main = defaultMain 
  [ bgroup "ccnf" [ bench "exp"   $ nf (flip nth exp)        n -- the first one always take longer!
                  , bench "exp"   $ nf (flip nth exp)        n
                  , bench "sine"  $ nf (flip nth $ sine 47)  n
                  , bench "sineP" $ nf (flip nth $ sineP 47) n]
  ] 
