{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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

type StockSeries =
  Record
    '["Date" :-> Day, "Price" :-> Double]

returnsStream :: (MonadSafe m) => Producer Returns m ()
returnsStream = readTableOpt returnsParser "data/Portfolios_Formed_on_ME_monthly_EW.csv"

drawdown ::
  StockSeries ->
  Record '[Date, Price, WealthIndex, PreviousPeak, Drawdown]
drawdown r = r <+> aux r
  where
    aux r = wealth :& priorPeak :& drawdownPercent :& RNil
    wealth = Identity (price ^ 2)
    priorPeak = Identity (price ^ 2)
    drawdownPercent = Identity (price ^ 2)
    price = rgetField # Price r
