{-# LANGUAGE TemplateHaskell #-}
{-# LINE 1 "Sound.as" #-}
module SoundTH where
{-# LINE 3 "Sound.as" #-}
import Control.CCA
{-# LINE 4 "Sound.as" #-}
import Prelude hiding (init)
{-# LINE 5 "Sound.as" #-}
import Language.Haskell.TH
{-# LINE 6 "Sound.as" #-}
import SoundAux
{-# LINE 7 "Sound.as" #-}
import System.Random.Mersenne.Pure64
{-# LINE 8 "Sound.as" #-}
import System.IO.Unsafe
{-# LINE 9 "Sound.as" #-}
import Data.Array.Unboxed
 
{-# LINE 11 "Sound.as" #-}
seghlp ::
       (ArrowInit a) =>
         [Double] -> [Double] -> a () (Double, Double, Double, Double)
{-# LINE 16 "Sound.as" #-}
seghlp iamps idurs
  = let {-# LINE 17 "Sound.as" #-}
        sr = rate (undefined)
        {-# LINE 18 "Sound.as" #-}
        sz = length iamps
        {-# LINE 19 "Sound.as" #-}
        amps = Tab iamps sz (listArray (0, sz - 1) iamps)
        {-# LINE 20 "Sound.as" #-}
        durs = Tab idurs (sz - 1) (listArray (0, sz - 2) (map (* sr) idurs))
      in
      (loop
         (((arr'
              [|
                (\ (_, ~(i, t)) ->
                   let {-# LINE 23 "Sound.as" #-}
                       durs = Tab idurs (sz - 1) (listArray (0, sz - 2) (map (* sr) idurs))
                       (t', i')
                         = if t >= durs `aAt` i then
                             if i == sz - 2 then (t + 1, i) else (0, i + 1) else (t + 1, i)
                     in (i', t'))
                |]
              (\ (_, ~(i, t)) ->
                 let {-# LINE 23 "Sound.as" #-}
                     (t', i')
                       = if t >= durs `aAt` i then
                           if i == sz - 2 then (t + 1, i) else (0, i + 1) else (t + 1, i)
                   in (i', t'))
              >>> first (init' [| 0 |] 0))
             >>> arr' [| (\ (i, t') -> (t', i)) |] (\ (i, t') -> (t', i)))
            >>>
            (first (init' [| 0 |] 0) >>>
               arr' [| (\ (t, i) -> ((i, t), (i, t))) |]
                 (\ (t, i) -> ((i, t), (i, t)))))
         >>>
         arr'
           [|
             (\ (i, t) ->
                let {-# LINE 28 "Sound.as" #-}
                    amps = Tab iamps sz (listArray (0, sz - 1) iamps)
                    a1 = aAt amps i
                    {-# LINE 29 "Sound.as" #-}
                    a2 = aAt amps (i + 1)
                    {-# LINE 30 "Sound.as" #-}
                    d = aAt durs i
                    durs = Tab idurs (sz - 1) (listArray (0, sz - 2) (map (* sr) idurs))
                  in (a1, a2, t, d))
             |]
           (\ (i, t) ->
              let {-# LINE 28 "Sound.as" #-}
                  a1 = aAt amps i
                  {-# LINE 29 "Sound.as" #-}
                  a2 = aAt amps (i + 1)
                  {-# LINE 30 "Sound.as" #-}
                  d = aAt durs i
                in (a1, a2, t, d)))
 
{-# LINE 34 "Sound.as" #-}
envLine ::
        (ArrowInit a) => Double -> Double -> Double -> a () Double
{-# LINE 39 "Sound.as" #-}
envLine a dur b
  = let {-# LINE 40 "Sound.as" #-}
        sr = rate undefined 
        srV = varE $ mkName "sr"
        aV = varE $ mkName "a"
        bV = varE $ mkName "b"
        durV = varE $ mkName "dur"
      in
      (loop
         (arr' [| (\ ((), y) -> y + ($(bV) - $(aV)) * (1 / $(srV) / $(durV))) |]
            (\ ((), y) -> y + (b - a) * (1 / sr / dur))
            >>>
            (init' [| $(aV) |] a >>> arr' [| (\ y -> (y, y)) |] (\ y -> (y, y)))))
 
{-# LINE 46 "Sound.as" #-}
envLineSeg :: (ArrowInit a) => [Double] -> [Double] -> a () Double
{-# LINE 51 "Sound.as" #-}
envLineSeg amps durs
  = let {-# LINE 52 "Sound.as" #-}
        sf = seghlp amps durs
      in
      (sf >>>
         arr' [| (\ (a1, a2, t, d) -> a1 + (a2 - a1) * (t / d)) |]
           (\ (a1, a2, t, d) -> a1 + (a2 - a1) * (t / d)))
 
{-# LINE 57 "Sound.as" #-}
noiseWhite :: (ArrowInit a) => Int -> a () Double
{-# LINE 58 "Sound.as" #-}
noiseWhite seed
  = let {-# LINE 59 "Sound.as" #-}
        gen = pureMT $ fromIntegral seed
      in
      (loop
         (arr'
            [|
              (\ ((), g) ->
                 let {-# LINE 62 "Sound.as" #-}
                     (a, g') = randomDouble g 
                   in (g', a))
              |]
            (\ ((), g) ->
               let {-# LINE 62 "Sound.as" #-}
                   (a, g') = randomDouble g 
                 in (g', a))
            >>>
            (first (init' [| let gen = pureMT (fromIntegral seed) in gen |] gen) >>>
               arr' [| (\ (g, a) -> (a, g)) |] (\ (g, a) -> (a, g))))
         >>> arr' [| (\ a -> a * 2 - 1) |] (\ a -> a * 2 - 1))
 
{-# LINE 66 "Sound.as" #-}
delayLine :: (ArrowInit a) => Double -> a Double Double
{-# LINE 68 "Sound.as" #-}
delayLine maxdel
  = let {-# LINE 69 "Sound.as" #-}
        sr = rate (undefined)
        {-# LINE 70 "Sound.as" #-}
        sz = truncate (sr * maxdel)
        {-# LINE 71 "Sound.as" #-}
        buf = mkArr sz
      in
      (loop
         (((arr'
              [|
                (\ (x, i) ->
                   let {-# LINE 74 "Sound.as" #-}
                       i' = if i == sz - 1 then 0 else i + 1
                     in (i', x))
                |]
              (\ (x, i) ->
                 let {-# LINE 74 "Sound.as" #-}
                     i' = if i == sz - 1 then 0 else i + 1
                   in (i', x))
              >>> first (init' [| 0 |] 0))
             >>> arr' [| (\ (i, x) -> (x, i)) |] (\ (i, x) -> (x, i)))
            >>>
            (first (init' [| 0 |] 0) >>>
               arr' [| (\ (y, i) -> ((i, y), i)) |] (\ (y, i) -> ((i, y), i))))
         >>>
         arr' [| let buf = mkArr sz in (\ (i, y) -> unsafePerformIO $ updateBuf buf i y) |]
           (\ (i, y) -> unsafePerformIO $ updateBuf buf i y))
 
{-# LINE 79 "Sound.as" #-}
butter :: (ArrowInit a) => a (Double, ButterData) Double
{-# LINE 80 "Sound.as" #-}
butter
  = (loop
       (((arr'
            [|
              (\ ((sig, ButterData a1 a2 a3 a4 a5), ~(y', y'')) ->
                 let {-# LINE 81 "Sound.as" #-}
                     t = sig - a4 * y' - a5 * y''
                     {-# LINE 82 "Sound.as" #-}
                     y = t * a1 + a2 * y' + a3 * y''
                   in (t, y))
              |]
            (\ ((sig, ButterData a1 a2 a3 a4 a5), ~(y', y'')) ->
               let {-# LINE 81 "Sound.as" #-}
                   t = sig - a4 * y' - a5 * y''
                   {-# LINE 82 "Sound.as" #-}
                   y = t * a1 + a2 * y' + a3 * y''
                 in (t, y))
            >>> first (init' [| 0 |] 0))
           >>>
           arr' [| (\ (y', y) -> (y', (y, y'))) |]
             (\ (y', y) -> (y', (y, y'))))
          >>>
          (first (init' [| 0 |] 0) >>>
             arr' [| (\ (y'', (y, y')) -> (y, (y', y''))) |]
               (\ (y'', (y, y')) -> (y, (y', y''))))))
 
{-# LINE 87 "Sound.as" #-}
filterLowPassBW :: (ArrowInit a) => a (Double, Double) Double
{-# LINE 88 "Sound.as" #-}
filterLowPassBW
  = let {-# LINE 89 "Sound.as" #-}
        sr = rate (undefined)
      in
      (arr' [| (\ (sig, freq) -> (sig, blpset freq sr)) |]
         (\ (sig, freq) -> (sig, blpset freq sr))
         >>> butter)
 
{-# LINE 93 "Sound.as" #-}
readFromSineTableA :: (ArrowInit a) => a Double Double
{-# LINE 94 "Sound.as" #-}
readFromSineTableA 
  = (arr' [| (\ x -> readFromTable sineTable x) |]
       (\ x -> readFromTable sineTable x))
 
{-# LINE 97 "Sound.as" #-}
oscSine :: (ArrowInit a) => Double -> a Double Double
{-# LINE 102 "Sound.as" #-}
oscSine iphs = osc_ iphs >>> readFromSineTableA 
 
{-# LINE 104 "Sound.as" #-}
osc_ :: (ArrowInit a) => Double -> a Double Double
{-# LINE 106 "Sound.as" #-}
osc_ phs
  = let {-# LINE 107 "Sound.as" #-}
        sr = rate (undefined)
      in
      (loop
         (arr'
            [|
              (\ (freq, next) ->
                 let {-# LINE 110 "Sound.as" #-}
                     delta = 1 / sr * freq
                     {-# LINE 111 "Sound.as" #-}
                     phase = if next > 1 then frac next else next
                   in ((delta, phase), phase))
              |]
            (\ (freq, next) ->
               let {-# LINE 110 "Sound.as" #-}
                   delta = 1 / sr * freq
                   {-# LINE 111 "Sound.as" #-}
                   phase = if next > 1 then frac next else next
                 in ((delta, phase), phase))
            >>>
            (first
               (arr' [| (\ (delta, phase) -> frac (phase + delta)) |]
                  (\ (delta, phase) -> frac (phase + delta))
                  >>> init' [| phs |] phs)
               >>>
               arr' [| (\ (next, phase) -> (phase, next)) |]
                 (\ (next, phase) -> (phase, next)))))
 
{-# LINE 115 "Sound.as" #-}
flute ::
      (ArrowInit a) =>
        Time -> Double -> Double -> Double -> Double -> a () Double
{-# LINE 117 "Sound.as" #-}
flute dur amp fqc press breath
  = ((envLineSeg [0, 1.1 * press, press, press, 0]
        [6.0e-2, 0.2, dur - 0.16, 2.0e-2]
        >>> arr' [| (\ env1 -> ((), env1)) |] (\ env1 -> ((), env1)))
       >>>
       (first (envLineSeg [0, 1, 1, 0] [1.0e-2, dur - 2.0e-2, 1.0e-2]) >>>
          arr' [| (\ (env2, env1) -> ((), (env1, env2))) |]
            (\ (env2, env1) -> ((), (env1, env2))))
         >>>
         (first (envLineSeg [0, 0, 1, 1] [0.5, 0.5, dur - 1]) >>>
            arr' [| (\ (envib, (env1, env2)) -> ((), (env1, env2, envib))) |]
              (\ (envib, (env1, env2)) -> ((), (env1, env2, envib))))
           >>>
           (first (noiseWhite 42) >>>
              arr'
                [|
                  (\ (flow, (env1, env2, envib)) -> ((), (env1, env2, envib, flow)))
                  |]
                (\ (flow, (env1, env2, envib)) -> ((), (env1, env2, envib, flow))))
             >>>
             ((first (arr' [| (\ () -> 5) |] (\ () -> 5) >>> oscSine 0)
                 >>>
                 arr'
                   [|
                     (\ (vib, (env1, env2, envib, flow)) ->
                        let {-# LINE 127 "Sound.as" #-}
                            emb = breath * flow * env1 + env1 + vib * 0.1 * envib
                          in (emb, env2))
                     |]
                   (\ (vib, (env1, env2, envib, flow)) ->
                      let {-# LINE 127 "Sound.as" #-}
                          emb = breath * flow * env1 + env1 + vib * 0.1 * envib
                        in (emb, env2)))
                >>>
                loop
                  (arr' [| (\ ((emb, env2), out) -> (out, (emb, env2))) |]
                     (\ ((emb, env2), out) -> (out, (emb, env2)))
                     >>>
                     (first (delayLine (1 / fqc)) >>>
                        arr'
                          [| (\ (flute, (emb, env2)) -> ((emb, flute), (env2, flute))) |]
                          (\ (flute, (emb, env2)) -> ((emb, flute), (env2, flute))))
                       >>>
                       (first
                          (arr' [| (\ (emb, flute) -> emb + flute * 0.4) |]
                             (\ (emb, flute) -> emb + flute * 0.4)
                             >>> delayLine (1 / fqc / 2))
                          >>>
                          arr' [| (\ (x, (env2, flute)) -> ((flute, x), env2)) |]
                            (\ (x, (env2, flute)) -> ((flute, x), env2)))
                         >>>
                         (first
                            (arr' [| (\ (flute, x) -> (x - x * x * x + flute * 0.4, 2000)) |]
                               (\ (flute, x) -> (x - x * x * x + flute * 0.4, 2000))
                               >>> filterLowPassBW)
                            >>>
                            arr' [| (\ (out, env2) -> ((env2, out), out)) |]
                              (\ (out, env2) -> ((env2, out), out)))))
               >>>
               arr' [| (\ (env2, out) -> out * amp * env2) |]
                 (\ (env2, out) -> out * amp * env2))
 
{-# LINE 133 "Sound.as" #-}
shepard :: (ArrowInit a) => Double -> a () Double
{-# LINE 134 "Sound.as" #-}
shepard seconds
  = if seconds <= 0.0 then ((arr' [| (\ _ -> 0.0) |] (\ _ -> 0.0)))
      else
      (arr' [| (\ _ -> ()) |] (\ _ -> ()) >>>
         (envLineSeg [800, 100, 100] [4.0, seconds] >>>
            arr' [| (\ f -> ((), f)) |] (\ f -> ((), f)))
           >>>
           (first (envLineSeg [0, 1, 0, 0] [2.0, 2.0, seconds]) >>>
              arr' [| (\ (e, f) -> (f, e)) |] (\ (e, f) -> (f, e)))
             >>>
             (first (oscSine 0) >>>
                arr' [| (\ (s, e) -> ((), (e, s))) |] (\ (s, e) -> ((), (e, s))))
               >>>
               (first (delayLine 0.5 <<< shepard (seconds - 0.5)) >>>
                  arr' [| (\ (sRest, (e, s)) -> (e * s * 0.1) + sRest) |]
                    (\ (sRest, (e, s)) -> (e * s * 0.1) + sRest)))

fluteA :: ArrowInit a => a () Double
fluteA = flute 5 0.3 440 0.99 0.2 

shepardA :: ArrowInit a => a () Double
shepardA =  shepard 16.0

rate _ = 44100

