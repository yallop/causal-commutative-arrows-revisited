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
exp_unnormalized inp elem = exp1 inp !! elem
exp1 = runSF exp

exp_normalized_b inp elem = exp2 inp !! elem
exp2 = runSF (observeB exp)

exp_normalized_d inp elem = exp3 inp !! elem
exp3 = runSF (observeD exp)

exp_normalized_direct_apply inp elem = exp4 inp !! elem
exp4 = applyCCNF_D exp

exp_normalized_th inp elem = exp5 inp !! elem
exp5 = applyCCNF $(normOpt S.exp)
applyCCNF :: (c, (a, c) -> (b, c)) -> [a] -> [b]
applyCCNF (i, f) = runCCNF i f
  where
   runCCNF :: e -> ((b,e) -> (c,e)) -> [b] -> [c]
   runCCNF i f = g i
     where g i (x:xs) = let (y, i') = f (x, i)
                        in y : g i' xs

exp_element = 30000

main :: IO ()
main = defaultMain [
  bgroup "exp" [ bench "exp (unnormalized)" $
                 nf (exp_unnormalized inp) exp_element

               , bench "exp (loopB-normalized)" $
                 nf (exp_normalized_b inp) exp_element

               , bench "exp (loopD-normalized)" $
                 nf (exp_normalized_d inp) exp_element

               , bench "exp (loopD-normalized, with runCCNF)" $
                 nf (exp_normalized_direct_apply inp) exp_element

               , bench "exp (loopD-normalized, with CCA TH)" $
                 nf (exp_normalized_th inp) exp_element
               ]
  ]
