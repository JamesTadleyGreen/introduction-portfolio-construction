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
import Data.Vinyl.Functor (Const (..), Identity (..))
import Frames
import Frames.CSV (readTableOpt)
import Frames.Rec (rgetField, rputField)
import Lens.Micro
import Lens.Micro.Extras
import Pipes (Producer, (>->))
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P
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

type PreviousPeak = "previous_peak" :-> Double

type Drawdown = "drawdown" :-> Double

type StockSeries =
  Record
    '[Date, Price]

type StockSeriesExtended = Record '[Date, Price, WealthIndex, PreviousPeak, Drawdown]

returnsStream :: (MonadSafe m) => Producer Returns m ()
returnsStream = readTableOpt returnsParser "data/Portfolios_Formed_on_ME_monthly_EW.csv"

loadReturns :: IO (Frame Returns)
loadReturns = inCoreAoS returnsStream

drawdown :: StockSeries -> StockSeriesExtended
drawdown r =
  let price = rgetField @Price r
      wealthIndex = price -- Replace with actual computation if needed
      priorPeak = price -- Replace with actual computation if needed
      drawdownPercent = (price - priorPeak) / priorPeak
      -- Using rputField to add fields to the record
      extendedRecord =
        r
          <+> wealthIndex
          &: priorPeak
          &: drawdownPercent
          &: RNil
   in extendedRecord

main :: IO ()
main = do
  ms <- loadReturns
  filtered <- rcast @'[Timestamp, Lo30] ms
  filtered
