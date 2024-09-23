{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ReturnsFrames (calculateDrawdown, WealthIndex, Peaks, Drawdown) where

import qualified Data.Foldable as F
import Data.Vinyl.TypeLevel
import Frames
import Frames.InCore (RecVec)

type WealthIndex = "wealth_index" :-> Double

type Peaks = "peaks" :-> Double

type Drawdown = "drawdown" :-> Double

type ExtendedRecord rs = '[WealthIndex, Peaks, Drawdown] ++ rs

calculateDrawdown :: forall col rs s. (col ~ '(s, Double), col ∈ rs, RecVec rs) => Frame (Record rs) -> Frame (Record (ExtendedRecord rs))
calculateDrawdown r = toFrame $ zipWith (\r (w, p, d) -> w &: p &: d &: r) (F.toList r) (zip3 wealth peaks drawdownValues)
  where
    col = fmap (rgetField @col) (F.toList r)
    wealth = tail $ scanl (\c p -> c * (1 + p / 100)) 1000 col
    peaks = tail $ scanl max 0 wealth
    drawdownValues = zipWith (\w p -> (w - p) / p) wealth peaks
