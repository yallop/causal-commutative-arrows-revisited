{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE BangPatterns, RankNTypes, TemplateHaskell, GADTs, TypeOperators, KindSignatures, Arrows #-}

module Main where

import Prelude hiding (init, exp, id, (.))
import Control.Arrow
import Control.Category
import Control.Applicative
import Control.Monad.ST
import Control.CCA.CCNF (normOpt)
import Control.CCA.Types
import Control.CCA.Instances
import Criterion.Main
import qualified Sample
import qualified SampleTH
import SigFun -- For continuation based SF implementation
import qualified Sound
import qualified SoundTH

nth :: Int -> SF () a -> a
nth n (SF f) = x `seq` if n == 0 then x else nth (n - 1) f'
  where (x, f') = f ()
{-# INLINE nth #-}

nthTH :: Int -> (b, ((), b) -> (a, b)) -> a
nthTH n (i, f) = aux n i
  where
    aux !n !i = x `seq` if n == 0 then x else aux (n-1) i'
      where (x, i') = f ((), i)
{-# INLINE nthTH #-}

nthCCNF_D :: Int -> CCNF_D () a -> a
nthCCNF_D n (ArrD f) = f ()
nthCCNF_D n (LoopD i f) = aux n i
  where
    aux !n !i = x `seq` if n == 0 then x else aux (n-1) i'
     where (x, i') = f ((), i)
{-# INLINE nthCCNF_D #-}

-- Need a wrapper to existentially quantify s before we can use runST
data NF_ST a b = NF_ST (forall s . CCNF_ST s a b)

nthST :: Int -> NF_ST () a -> a
nthST n (NF_ST nf) = runST (nthST' n nf)
{-# INLINE nthST #-}

nthST' :: Int -> CCNF_ST s () a -> ST s a
nthST' n (ArrST f) = return (f ())
nthST' n (LoopST i f) = do
  g <- (f $) <$> i
  let next n = do 
      x <- g ()
      x `seq` if n <= 0 then return x else next (n-1)
  next n
{-# INLINE nthST' #-}

-- various approaches to evaluating 'exp'
exp_unnormalized elem = nth elem Sample.exp 

exp_normalized_b elem = nth elem (observeB Sample.exp)

exp_normalized_d elem = nth elem (observeD Sample.exp)

exp_normalized_nf elem = nthCCNF_D elem Sample.exp

exp_normalized_st elem = nthST elem $ NF_ST Sample.exp

exp_normalized_th elem = nthTH elem $(normOpt SampleTH.exp)


-- various approaches to evaluating 'sine'
sine_unnormalized :: Int -> Double
sine_unnormalized elem = nth elem (Sample.sine 47.0)

sine_normalized_b :: Int -> Double
sine_normalized_b elem = nth elem (observeB (Sample.sine 47.0))

sine_normalized_d :: Int -> Double
sine_normalized_d elem = nth elem (observeD (Sample.sine 47.0))

sine_normalized_nf :: Int -> Double
sine_normalized_nf elem = nthCCNF_D elem (Sample.sine 47.0)

sine_normalized_st :: Int -> Double
sine_normalized_st elem = nthST elem $ NF_ST (Sample.sine 47.0)

sine_normalized_th :: Int -> Double
sine_normalized_th elem = nthTH elem $(normOpt (SampleTH.sine 47.0))

-- various approaches to evaluating 'fibA'
fibA_unnormalized :: Int -> Integer
fibA_unnormalized elem = nth elem Sample.fibA

fibA_normalized_b :: Int -> Integer
fibA_normalized_b elem = nth elem (observeB Sample.fibA)

fibA_normalized_d :: Int -> Integer
fibA_normalized_d elem = nth elem (observeD Sample.fibA)

fibA_normalized_nf :: Int -> Integer
fibA_normalized_nf elem = nthCCNF_D elem Sample.fibA

fibA_normalized_st :: Int -> Integer
fibA_normalized_st elem = nthST elem $ NF_ST Sample.fibA

fibA_normalized_th :: Int -> Integer
fibA_normalized_th elem = nthTH elem $(normOpt SampleTH.fibA)

-- various approaches to evaluating 'oscSineA'
oscSineA_unnormalized elem = nth elem Sample.oscSineA 

oscSineA_normalized_b elem = nth elem (observeB Sample.oscSineA)

oscSineA_normalized_d elem = nth elem (observeD Sample.oscSineA)

oscSineA_normalized_nf elem = nthCCNF_D elem Sample.oscSineA

oscSineA_normalized_st elem = nthST elem $ NF_ST Sample.oscSineA

oscSineA_normalized_th elem = nthTH elem $(normOpt SampleTH.oscSineA)

-- various approaches to evaluating 'sciFi'
sciFi_unnormalized elem = nth elem Sample.sciFi 

sciFi_normalized_b elem = nth elem (observeB Sample.sciFi)

sciFi_normalized_d elem = nth elem (observeD Sample.sciFi)

sciFi_normalized_nf elem = nthCCNF_D elem Sample.sciFi

sciFi_normalized_st elem = nthST elem $ NF_ST Sample.sciFi

sciFi_normalized_th elem = nthTH elem $(normOpt SampleTH.sciFi)

-- various approaches to evaluating 'robotA'
robotA_unnormalized elem = nth elem Sample.robotA 

robotA_normalized_b elem = nth elem (observeB Sample.robotA)

robotA_normalized_d elem = nth elem (observeD Sample.robotA)

robotA_normalized_nf elem = nthCCNF_D elem Sample.robotA

robotA_normalized_st elem = nthST elem $ NF_ST Sample.robotA

robotA_normalized_th elem = nthTH elem $(normOpt SampleTH.robotA)

-- various approaches to evaluating 'flute'
flute :: ArrowInitLine a => a () Double 
flute = Sound.flute 5 0.3 440 0.99 0.2

flute_unnormalized elem = nth elem flute

-- flute_normalized_b elem = nth elem (observeB flute)

flute_normalized_d elem = nth elem (observeD flute)

flute_normalized_nf elem = nthCCNF_D elem flute

flute_normalized_st elem = nthST elem $ NF_ST flute

flute_normalized_th elem = nthTH elem $(normOpt $ SoundTH.flute 5 0.3 440 0.99 0.2)

-- various approaches to evaluating 'shepard'
shepard :: ArrowInitLine a => a () Double 
shepard = Sound.shepard 5 

shepard_unnormalized elem = nth elem shepard

-- shepard_normalized_b elem = nth elem (observeB shepard)

shepard_normalized_d elem = nth elem (observeD shepard)

shepard_normalized_nf elem = nthCCNF_D elem shepard

shepard_normalized_st elem = nthST elem $ NF_ST shepard

shepard_normalized_th elem = nthTH elem $(normOpt $ SoundTH.shepard 5)

-- how many elements to evaluate
n = 44100 * 5
exp_element = n
fibA_element = n
sine_element = n
oscSineA_element = n
sciFi_element = n
robotA_element = n
flute_element = n
shepard_element = n

main :: IO ()
main = do
 defaultMain [
  bgroup "exp" [ bench "exp (unnormalized)" $
                 nf (exp_unnormalized) exp_element

               , bench "exp (loopD-normalized)" $
                 nf (exp_normalized_d) exp_element

               , bench "exp (loopD-normalized, with nthCCNF)" $
                 nf (exp_normalized_nf) exp_element

               , bench "exp (loopD-normalized, with nthST)" $
                 nf (exp_normalized_st) exp_element

               , bench "exp (loopD-normalized, with CCA TH)" $
                 nf (exp_normalized_th) exp_element
               ],

  bgroup "sine" [ bench "sine (unnormalized)" $
                 nf (sine_unnormalized) sine_element

               , bench "sine (loopD-normalized)" $
                 nf (sine_normalized_d) sine_element

               , bench "sine (loopD-normalized, with nthCCNF)" $
                 nf (sine_normalized_nf) sine_element

               , bench "sine (loopD-normalized, with nthST)" $
                 nf (sine_normalized_st) sine_element

               , bench "sine (loopD-normalized, with CCA TH)" $
                 nf (sine_normalized_th) sine_element
               ],

  bgroup "fibA" [ bench "fibA (unnormalized)" $
                 nf (fibA_unnormalized) fibA_element

                 , bench "fibA (loopD-normalized)" $
                 nf (fibA_normalized_d) fibA_element

               , bench "fibA (loopD-normalized, with nthCCNF)" $
                 nf (fibA_normalized_nf) fibA_element

               , bench "fibA (loopD-normalized, with nthST)" $
                 nf (fibA_normalized_st) fibA_element

               , bench "fibA (loopD-normalized, with CCA TH)" $
                 nf (fibA_normalized_th) fibA_element
               ],

  bgroup "oscSineA" [ bench "oscSineA (unnormalized)" $
                 nf (oscSineA_unnormalized) oscSineA_element

               , bench "oscSineA (loopD-normalized)" $
                 nf (oscSineA_normalized_d) oscSineA_element

               , bench "oscSineA (loopD-normalized, with nthCCNF)" $
                 nf (oscSineA_normalized_nf) oscSineA_element

               , bench "oscSineA (loopD-normalized, with nthST)" $
                 nf (oscSineA_normalized_st) oscSineA_element

               , bench "oscSineA (loopD-normalized, with CCA TH)" $
                 nf (oscSineA_normalized_th) oscSineA_element
               ],

  bgroup "sciFi" [ bench "sciFi (unnormalized)" $
                 nf (sciFi_unnormalized) sciFi_element

               , bench "sciFi (loopD-normalized)" $
                 nf (sciFi_normalized_d) sciFi_element

               , bench "sciFi (loopD-normalized, with nthCCNF)" $
                 nf (sciFi_normalized_nf) sciFi_element

               , bench "sciFi (loopD-normalized, with nthST)" $
                 nf (sciFi_normalized_st) sciFi_element

               , bench "sciFi (loopD-normalized, with CCA TH)" $
                 nf (sciFi_normalized_th) sciFi_element
               ],


  bgroup "robotA" [ bench "robotA (unnormalized)" $
                 nf (robotA_unnormalized) robotA_element

               , bench "robotA (loopD-normalized)" $
                 nf (robotA_normalized_d) robotA_element

               , bench "robotA (loopD-normalized, with nthCCNF)" $
                 nf (robotA_normalized_nf) robotA_element

               , bench "robotA (loopD-normalized, with nthST)" $
                 nf (robotA_normalized_st) robotA_element

               , bench "robotA (loopD-normalized, with CCA TH)" $
                 nf (robotA_normalized_th) robotA_element
               ],

  bgroup "flute" [ bench "flute (unnormalized)" $
                 nf (flute_unnormalized) flute_element

               , bench "flute (loopD-normalized)" $
                 nf (flute_normalized_d) flute_element

               , bench "flute (loopD-normalized, with nthCCNF)" $
                 nf (flute_normalized_nf) flute_element

               , bench "flute (loopD-normalized, with nthST)" $
                 nf (flute_normalized_st) flute_element

               , bench "flute (loopD-normalized, with CCA TH)" $
                 nf (flute_normalized_th) flute_element
               ],

  bgroup "shepard" [ bench "shepard (unnormalized)" $
                 nf (shepard_unnormalized) shepard_element

               , bench "shepard (loopD-normalized)" $
                 nf (shepard_normalized_d) shepard_element

               , bench "shepard (loopD-normalized, with nthCCNF)" $
                 nf (shepard_normalized_nf) shepard_element

               , bench "shepard (loopD-normalized, with nthST)" $
                 nf (shepard_normalized_st) shepard_element

               , bench "shepard (loopD-normalized, with CCA TH)" $
                 nf (shepard_normalized_th) shepard_element
               ]


  ]
