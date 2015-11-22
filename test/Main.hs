{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell, GADTs, TypeOperators, KindSignatures #-}

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
{-# INLINE integral #-}

dt :: Double
dt = 0.0001
{-# INLINE dt #-}

inp :: [()]
inp = () : inp

fixA :: ArrowLoop arr => a `arr` a -> b `arr` a
fixA = \f -> loop (second f >>> arr snd >>> arr dup)
{-# INLINE fixA #-}

exp :: ArrowInit arr => () `arr` Double
exp = fixA (integral >>> arr (+1))
{-# INLINE exp #-}

-- Causal stream transformers 
newtype SF a b = SF { unSF :: a -> (b, SF a b) }

instance Category SF where
  id = arr id

  g . f = SF (h f g)
    where h (SF f) (SF g) x = let (y, f') = f x
                                  (z, g') = g y
                               in (z, SF (h f' g'))
  {-# INLINE (.) #-}

instance Arrow SF where
  arr f = SF h
    where h x = (f x, SF h)
  {-# INLINE arr #-}
  first f = SF (h f)
    where h (SF f) (x, z) = let (y, f') = f x  
                             in ((y,z), SF (h f'))
  {-# INLINE first #-}

instance ArrowLoop SF where
   loop = \f -> SF (h f)
      where h (SF f) x = let ((y, z), f') = f (x, z)
                          in (y, SF (h f'))
   {-# INLINE loop #-}

instance ArrowInit SF where
  init = \i -> SF (h i)
    where h i x = (i, SF (h x))
  {-# INLINE init #-}

runSF :: SF a b -> [a] -> [b]
runSF (SF f) (x:xs) = let (y, f') = f x
                      in y : runSF f' xs

-- various approaches to evaluating 'exp'
exp_unnormalized elem = nth elem exp 

exp_normalized_b elem = nth elem (observeB exp)

exp_normalized_d elem = nth elem (observeD exp)

exp_normalized_direct_apply elem = nthCCNF_D elem exp

exp_normalized_th_nth elem = nth' elem $(normOpt S.exp)

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

main :: IO ()
main = defaultMain [
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
               ]
  ]
