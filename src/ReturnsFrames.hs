{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module ReturnsFrames where

import qualified Control.Foldl as L
import qualified Data.Foldable as F
import qualified Data.Ord as V
import Data.Time (Day)
import qualified Data.Vector as V
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Frames
import Frames.CSV (readTableOpt)
import Frames.InCore (RecVec)
import Frames.Rec
import Lens.Micro
import Lens.Micro.Extras
import Pipes (Producer, (>->))
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P
import qualified ReturnsVectors as Returns
import Statistics.Sample as S

-- define a Show instance for frames
instance (Show a) => Show (Frame a) where
  show (Frame l f) =
    show (f 0)
      ++ (if l > 1 then "\n" ++ show (f 1) else "")
      ++ (if l > 2 then "\n..." else "")
      ++ "\nFrame with "
      ++ show l
      ++ if l > 1 then " rows." else " row."

tableTypes "Returns" "data/Portfolios_Formed_on_ME_monthly_EW.csv"

type Date = "date" :-> Day

type Price = "price" :-> Double

type WealthIndex = "wealth_index" :-> Double

type Peaks = "peaks" :-> Double

type PreviousPeak = "previous_peak" :-> Double

type Drawdown = "drawdown" :-> Double

type PercentChange = "percent_change" :-> Double

type StockSeries =
  Record
    '[Timestamp, Hi10]

type StockSeriesExtended = Record '[Timestamp, Hi10, WealthIndex, Peaks, Drawdown]

returnsStream :: (MonadSafe m) => Producer Returns m ()
returnsStream = readTableOpt returnsParser "data/Portfolios_Formed_on_ME_monthly_EW.csv"

loadReturns :: IO (Frame Returns)
loadReturns = inCoreAoS returnsStream

-- computePercentChange :: Double -> Double
-- computePercentChange x = x * 1.10

-- addPercentChange :: Frame StockSeries -> Frame StockSeriesExtended
-- addPercentChange frame =
--   let percentChanges = fmap (computePercentChange . rgetField @Hi10) frame
--       percentChangeFrame = toFrame $ fmap (&: RNil) percentChanges
--    in zipFrames frame percentChangeFrame

drawdown :: Frame StockSeries -> Frame StockSeriesExtended
drawdown r = zipFrames r wealthIndexFrame
  where
    lo30 = map (rgetField @Hi10) (F.toList r)
    wealth = tail $ scanl (\c p -> c * (1 + p / 100)) 1000 lo30
    peaks = tail $ scanl max 0 wealth
    drawdown = zipWith (\w p -> (w - p) / p) wealth peaks
    wealthIndexFrame = toFrame $ fmap (\(w, p, d) -> w &: p &: d &: RNil) (zip3 wealth peaks drawdown)

main :: IO ()
main = do
  ms <- loadReturns
  let casted_ms = fmap (rcast :: Returns -> StockSeries) ms
  let filtered_ms = drawdown casted_ms
  printFrame ", " filtered_ms
