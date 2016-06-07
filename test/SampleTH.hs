{-# LANGUAGE TemplateHaskell #-}

module SampleTH where

import Control.CCA

import Prelude hiding (init, exp)

import Language.Haskell.TH

sr = 44100 :: Int

dt = 1 / (fromIntegral sr)
 

exp :: (ArrowInit a) => a () Double

exp
  = (loop
       (arr'
          [|
            (\ ((), i) ->
               let 
                   e = 1 + i
                 in (e, e))
            |]
          (\ ((), i) ->
             let 
                 e = 1 + i
               in (e, e))
          >>>
          (first integral >>>
             arr' [| (\ (i, e) -> (e, i)) |] (\ (i, e) -> (e, i)))))
 

integral :: (ArrowInit a) => a Double Double

integral
  = (loop
       ((arr'
           [|
             (\ (x, i) ->
                let 
                    i' = i + x * dt
                  in i')
             |]
           (\ (x, i) ->
              let 
                  i' = i + x * dt
                in i')
           >>> init' [| 0 |] 0)
          >>> arr' [| (\ i -> (i, i)) |] (\ i -> (i, i))))
 

sine :: (ArrowInit a) => Double -> a () Double

sine freq
  = (loop
       (arr' [| (\ (_, r) -> r) |] (\ (_, r) -> r) >>>
          (init' [| i |] i >>> arr' [| (\ x -> (x, x)) |] (\ x -> (x, x)))
            >>>
            (first (init' [| 0 |] 0) >>>
               arr'
                 [|
                   (\ (y, x) ->
                      let 
                          r = c * x - y
                        in (r, r))
                   |]
                 (\ (y, x) ->
                    let 
                        r = c * x - y
                      in (r, r)))))
  where 
        omh = 2 * pi / (fromIntegral sr) * freq
        
        i = sin omh
        
        c = 2 * cos omh
 

oscSine :: (ArrowInit a) => Double -> a Double Double

oscSine f0
  = (arr'
       [|
         (\ cv ->
            let 
                f = f0 * (2 ** cv)
              in 2 * pi * f)
         |]
       (\ cv ->
          let 
              f = f0 * (2 ** cv)
            in 2 * pi * f)
       >>>
       (integral >>> arr' [| (\ phi -> sin phi) |] (\ phi -> sin phi)))
 

testOsc ::
        (ArrowInit a) => (Double -> a Double Double) -> a () Double

testOsc f = constant 1 >>> f 440

oscSineA :: (ArrowInit a) => a () Double
oscSineA = testOsc oscSine 

sciFi :: (ArrowInit a) => a () Double

sciFi
  = (arr' [| (\ () -> 0) |] (\ () -> 0) >>>
       (oscSine 3.0 >>>
          arr' [| (\ und -> ((), und)) |] (\ und -> ((), und)))
         >>>
         (first (arr' [| (\ () -> - 0.25) |] (\ () -> - 0.25) >>> integral)
            >>>
            arr' [| (\ (swp, und) -> und * 0.2 + swp + 1) |]
              (\ (swp, und) -> und * 0.2 + swp + 1))
           >>> oscSine 440)
 

robot :: (ArrowInit a) => a (Double, Double) Double

robot
  = (arr'
       [|
         (\ inp ->
            let 
                vr = snd inp
                
                vl = fst inp
                
                vz = vr + vl
              in ((vl, vr), vz))
         |]
       (\ inp ->
          let 
              vr = snd inp
              
              vl = fst inp
              
              vz = vr + vl
            in ((vl, vr), vz))
       >>>
       (first
          (arr' [| (\ (vl, vr) -> vr - vl) |] (\ (vl, vr) -> vr - vl) >>>
             integral)
          >>>
          arr'
            [|
              (\ (t, vz) ->
                 let 
                     t' = t / 10
                   in ((t', vz), (t', vz)))
              |]
            (\ (t, vz) ->
               let 
                   t' = t / 10
                 in ((t', vz), (t', vz))))
         >>>
         (first
            (arr' [| (\ (t', vz) -> vz * cos t') |] (\ (t', vz) -> vz * cos t')
               >>> integral)
            >>>
            arr' [| (\ (x, (t', vz)) -> ((t', vz), x)) |]
              (\ (x, (t', vz)) -> ((t', vz), x)))
           >>>
           (first
              (arr' [| (\ (t', vz) -> vz * sin t') |] (\ (t', vz) -> vz * sin t')
                 >>> integral)
              >>>
              arr' [| (\ (y, x) -> x / 2 + y / 2) |]
                (\ (y, x) -> x / 2 + y / 2)))
 

testRobot ::
          (ArrowInit a) => a (Double, Double) Double -> a () Double

testRobot bot
  = ((sine 2 >>> arr' [| (\ u -> (u, 1 - u)) |] (\ u -> (u, 1 - u)))
       >>> robot)

robotA :: (ArrowInit a) => a () Double
robotA = testRobot robot

fibA :: (ArrowInit arr) => arr () Integer

fibA
  = (loop
       (((arr'
            [|
              (\ (_, ~(d1, d2)) ->
                 let 
                     r = d2 + d1
                   in (d2, r))
              |]
            (\ (_, ~(d1, d2)) ->
               let 
                   r = d2 + d1
                 in (d2, r))
            >>> first (init' [| 0 |] 0))
           >>>
           arr' [| (\ (d1, r) -> (r, (d1, r))) |] (\ (d1, r) -> (r, (d1, r))))
          >>>
          (first (init' [| 1 |] 1) >>>
             arr' [| (\ (d2, (d1, r)) -> (r, (d1, d2))) |]
               (\ (d2, (d1, r)) -> (r, (d1, d2))))))
