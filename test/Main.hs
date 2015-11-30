{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell, GADTs, TypeOperators, KindSignatures, Arrows #-}

module Main where

import Prelude hiding (init, exp, id, (.))
import Control.Arrow
import Control.Category
import Control.CCA.CCNF (normOpt)
import Control.CCA.Instances
import Criterion.Main
import qualified Sample as S

integral :: ArrowInit arr => Double `arr` Double
integral = loop (arr (\ (v, i) -> i + dt * v) >>>
                 init 0 >>> arr dup)

sr = 44100 :: Int
dt = 1 / (fromIntegral sr)

inp :: [()]
inp = () : inp

fixA :: ArrowLoop arr => a `arr` a -> b `arr` a
fixA = \f -> loop (second f >>> arr snd >>> arr dup)

exp :: ArrowInit arr => () `arr` Double
exp = fixA (integral >>> arr (+1))

fibA :: ArrowInit arr => arr () Integer
fibA = proc _ -> do
   rec let r = d2 + d1
       d1 <- init 0 -< d2
       d2 <- init 1 -< r
   returnA -< r

sine :: ArrowInit a => Double -> a () Double
sine freq = proc _ -> do
  rec x <- init i -< r
      y <- init 0 -< x 
      let r = c * x - y
  returnA -< r
  where
    omh = 2 * pi / (fromIntegral sr) * freq
    i = sin omh
    c = 2 * cos omh
{-# INLINE sine #-}

-- Causal stream transformers 
newtype SF a b = SF { unSF :: a -> (b, SF a b) }

instance Category SF where
  id = arr id

  g . f = SF (h f g)
    where h (SF f) (SF g) x = let (y, f') = f x
                                  (z, g') = g y
                               in (z, SF (h f' g'))

instance Arrow SF where
  arr f = SF h
    where h x = (f x, SF h)
  first f = SF (h f)
    where h (SF f) (x, z) = let (y, f') = f x  
                             in ((y,z), SF (h f'))

instance ArrowLoop SF where
   loop = \f -> SF (h f)
      where h (SF f) x = let ((y, z), f') = f (x, z)
                          in (y, SF (h f'))

instance ArrowInit SF where
  init = \i -> SF (h i)
    where h i x = (i, SF (h x))

runSF :: SF a b -> [a] -> [b]
runSF (SF f) (x:xs) = let (y, f') = f x
                      in y : runSF f' xs

-- various approaches to evaluating 'exp'
exp_unnormalized elem = nth elem exp 

exp_normalized_b elem = nth elem (observeB exp)

exp_normalized_d elem = nth elem (observeD exp)

exp_normalized_direct_apply elem = nthCCNF_D elem exp

exp_normalized_th_nth elem = nth' elem $(normOpt S.exp)


-- various approaches to evaluating 'sine'
sine_unnormalized :: Int -> Double
sine_unnormalized elem = nth elem (sine 47.0)

sine_normalized_b :: Int -> Double
sine_normalized_b elem = nth elem (observeB (sine 47.0))

sine_normalized_d :: Int -> Double
sine_normalized_d elem = nth elem (observeD (sine 47.0))

sine_normalized_direct_apply :: Int -> Double
sine_normalized_direct_apply elem = nthCCNF_D elem (sine 47.0)

sine_normalized_th_nth :: Int -> Double
sine_normalized_th_nth elem = nth' elem $(normOpt (S.sine 47.0))

-- various approaches to evaluating 'fibA'
fibA_unnormalized :: Int -> Integer
fibA_unnormalized elem = nth elem fibA

fibA_normalized_b :: Int -> Integer
fibA_normalized_b elem = nth elem (observeB fibA)

fibA_normalized_d :: Int -> Integer
fibA_normalized_d elem = nth elem (observeD fibA)

fibA_normalized_direct_apply :: Int -> Integer
fibA_normalized_direct_apply elem = nthCCNF_D elem fibA

fibA_normalized_th_nth :: Int -> Integer
fibA_normalized_th_nth elem = nth' elem $(normOpt S.fibA)

nth :: Int -> SF () a -> a
nth n (SF f) = x `seq` if n == 0 then x else nth (n - 1) f'
  where (x, f') = f ()

nth' :: Int -> (b, ((), b) -> (a, b)) -> a
nth' n (i, f) = aux n i
  where
    aux n i = x `seq` if n == 0 then x else aux (n-1) i'
      where (x, i') = f ((), i)

nthCCNF_D :: Int -> CCNF_D () a -> a
nthCCNF_D n (ArrD f) = f ()
nthCCNF_D n (LoopD i f) = aux n i
  where
    aux n i = x `seq` if n == 0 then x else aux (n-1) i'
     where (x, i') = f ((), i)


exp_element = 30000
fibA_element = 30000
sine_element = 30000

main :: IO ()
main = do
 defaultMain [
  bgroup "exp" [ bench "exp (unnormalized)" $
                 nf (exp_unnormalized) exp_element

               , bench "exp (loopB-normalized)" $
                 nf (exp_normalized_b) exp_element

               , bench "exp (loopD-normalized)" $
                 nf (exp_normalized_d) exp_element

               , bench "exp (loopD-normalized, with runCCNF)" $
                 nf (exp_normalized_direct_apply) exp_element

               , bench "exp (loopD-normalized, with CCA TH)" $
                 nf (exp_normalized_th_nth) exp_element
               ],

  bgroup "sine" [ bench "sine (unnormalized)" $
                 nf (sine_unnormalized) sine_element

               , bench "sine (loopB-normalized)" $
                 nf (sine_normalized_b) sine_element

               , bench "sine (loopD-normalized)" $
                 nf (sine_normalized_d) sine_element

               , bench "sine (loopD-normalized, with runCCNF)" $
                 nf (sine_normalized_direct_apply) sine_element

               , bench "sine (loopD-normalized, with CCA TH)" $
                 nf (sine_normalized_th_nth) sine_element
               ],

  bgroup "fibA" [ bench "fibA (unnormalized)" $
                 nf (fibA_unnormalized) fibA_element

               , bench "fibA (loopB-normalized)" $
                 nf (fibA_normalized_b) fibA_element

               , bench "fibA (loopD-normalized)" $
                 nf (fibA_normalized_d) fibA_element

               , bench "fibA (loopD-normalized, with runCCNF)" $
                 nf (fibA_normalized_direct_apply) fibA_element

               , bench "fibA (loopD-normalized, with CCA TH)" $
                 nf (fibA_normalized_th_nth) fibA_element
               ]
  ]
