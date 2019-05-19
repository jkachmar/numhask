{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import NumHask.Array
import NumHask.Prelude
import Perf
import System.Random.MWC
-- import Perf.Criterion
import Perf.Analysis
import Criterion.Main
-- import qualified Criterion.Measurement as C


cri f a = defaultMain [
  bgroup "cri" [ bench "cri1"  $ whnf f a
               ]
  ]

fib :: Int -> Int
fib m = go m
  where
    go :: Int -> Int
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

main' :: IO ()
main' = do
  _ <- warmup 100
  -- sz = 10 run
  g <- create
  ra <- sequence $ replicate 100 (uniform g :: IO Double)
  rb <- sequence $ replicate 100 (uniform g :: IO Double)
  let aa = fromList ra :: Array [] '[10, 10] Double
  let ab = fromList rb :: Array [] '[10, 10] Double
  -- (t, res) <- ticks 1 (NumHask.Array.mmult aa) ab
  -- cri (fib :: Int -> Int) (7::Int)
  -- pc1 <- criNF 100000 (NumHask.Array.mmult aa) ab
  -- print $ Perf.Criterion.formatRun "cri: " 2 $ pc1
  print $ sum $ mmult aa ab
  -- print $ (fromIntegral $ sum t :: Double) / 10000000

{-

-}
main = do
  g <- create
  ra <- sequence $ replicate 100 (uniform g :: IO Double)
  rb <- sequence $ replicate 100 (uniform g :: IO Double)
  let aa = fromList ra :: Array [] '[10, 10] Double
  let ab = fromList rb :: Array [] '[10, 10] Double
  defaultMain [
    bgroup "cri"
    [ bench "nf numhask [] mmult"  $ nf (mmult aa) ab
    , bench "nf numhask [] (NumHask.Array.><)"  $ nf ((NumHask.Array.><) aa) ab
    , bench "nf numhask [] (NumHask.Prelude.><)"  $ nf ((NumHask.Prelude.><) aa) ab
    , bench "nf numhask [] sum . mmult"  $ nf (sum . mmult aa) ab
    , bench "nf numhask [] sum"  $ nf sum (mmult aa ab)
    ]
    ]


-- mmult aa ab
-- 
