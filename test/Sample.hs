{-# LANGUAGE TemplateHaskell #-}
{-# LINE 1 "Sample.as" #-}
module Sample where
{-# LINE 3 "Sample.as" #-}
import Control.CCA
{-# LINE 4 "Sample.as" #-}
import Prelude hiding (init, exp)
{-# LINE 5 "Sample.as" #-}
import Language.Haskell.TH
{-# LINE 7 "Sample.as" #-}
sr = 44100 :: Int
{-# LINE 8 "Sample.as" #-}
dt = 1 / (fromIntegral sr)
 
{-# LINE 10 "Sample.as" #-}
exp :: (ArrowInit a) => a () Double
{-# LINE 11 "Sample.as" #-}
exp
  = (loop
       (arr'
          [|
            (\ ((), i) ->
               let {-# LINE 12 "Sample.as" #-}
                   e = 1 + i
                 in (e, e))
            |]
          (\ ((), i) ->
             let {-# LINE 12 "Sample.as" #-}
                 e = 1 + i
               in (e, e))
          >>>
          (first integral >>>
             arr' [| (\ (i, e) -> (e, i)) |] (\ (i, e) -> (e, i)))))
 
{-# LINE 16 "Sample.as" #-}
integral :: (ArrowInit a) => a Double Double
{-# LINE 17 "Sample.as" #-}
integral
  = (loop
       ((arr'
           [|
             (\ (x, i) ->
                let {-# LINE 18 "Sample.as" #-}
                    i' = i + x * dt
                  in i')
             |]
           (\ (x, i) ->
              let {-# LINE 18 "Sample.as" #-}
                  i' = i + x * dt
                in i')
           >>> init' [| 0 |] 0)
          >>> arr' [| (\ i -> (i, i)) |] (\ i -> (i, i))))
 
{-# LINE 22 "Sample.as" #-}
sine :: (ArrowInit a) => Double -> a () Double
{-# LINE 23 "Sample.as" #-}
sine freq
  = (loop
       (arr' [| (\ (_, r) -> r) |] (\ (_, r) -> r) >>>
          (init' [| i |] i >>> arr' [| (\ x -> (x, x)) |] (\ x -> (x, x)))
            >>>
            (first (init' [| 0 |] 0) >>>
               arr'
                 [|
                   (\ (y, x) ->
                      let {-# LINE 26 "Sample.as" #-}
                          r = c * x - y
                        in (r, r))
                   |]
                 (\ (y, x) ->
                    let {-# LINE 26 "Sample.as" #-}
                        r = c * x - y
                      in (r, r)))))
  where {-# LINE 29 "Sample.as" #-}
        omh = 2 * pi / (fromIntegral sr) * freq
        {-# LINE 30 "Sample.as" #-}
        i = sin omh
        {-# LINE 31 "Sample.as" #-}
        c = 2 * cos omh
 
{-# LINE 33 "Sample.as" #-}
oscSine :: (ArrowInit a) => Double -> a Double Double
{-# LINE 34 "Sample.as" #-}
oscSine f0
  = (arr'
       [|
         (\ cv ->
            let {-# LINE 35 "Sample.as" #-}
                f = f0 * (2 ** cv)
              in 2 * pi * f)
         |]
       (\ cv ->
          let {-# LINE 35 "Sample.as" #-}
              f = f0 * (2 ** cv)
            in 2 * pi * f)
       >>>
       (integral >>> arr' [| (\ phi -> sin phi) |] (\ phi -> sin phi)))
 
{-# LINE 39 "Sample.as" #-}
testOsc ::
        (ArrowInit a) => (Double -> a Double Double) -> a () Double
{-# LINE 40 "Sample.as" #-}
testOsc f = constant 1 >>> f 440
 
{-# LINE 42 "Sample.as" #-}
sciFi :: (ArrowInit a) => a () Double
{-# LINE 43 "Sample.as" #-}
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
 
{-# LINE 49 "Sample.as" #-}
robot :: (ArrowInit a) => a (Double, Double) Double
{-# LINE 50 "Sample.as" #-}
robot
  = (arr'
       [|
         (\ inp ->
            let {-# LINE 51 "Sample.as" #-}
                vr = snd inp
                {-# LINE 52 "Sample.as" #-}
                vl = fst inp
                {-# LINE 53 "Sample.as" #-}
                vz = vr + vl
              in ((vl, vr), vz))
         |]
       (\ inp ->
          let {-# LINE 51 "Sample.as" #-}
              vr = snd inp
              {-# LINE 52 "Sample.as" #-}
              vl = fst inp
              {-# LINE 53 "Sample.as" #-}
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
                 let {-# LINE 55 "Sample.as" #-}
                     t' = t / 10
                   in ((t', vz), (t', vz)))
              |]
            (\ (t, vz) ->
               let {-# LINE 55 "Sample.as" #-}
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
 
{-# LINE 60 "Sample.as" #-}
testRobot ::
          (ArrowInit a) => a (Double, Double) Double -> a () Double
{-# LINE 61 "Sample.as" #-}
testRobot bot
  = ((sine 2 >>> arr' [| (\ u -> (u, 1 - u)) |] (\ u -> (u, 1 - u)))
       >>> robot)
