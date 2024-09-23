{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ReturnsData where

import qualified Control.Foldl as F
import qualified Data.Foldable as Fold
import Data.List as L
import Frames
import Lens.Micro.Extras
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P
import ReturnsFrames

tableTypes "Returns" "data/Portfolios_Formed_on_ME_monthly_EW.csv"

returnsStream :: (MonadSafe m) => Producer Returns m ()
returnsStream = readTableOpt returnsParser "data/Portfolios_Formed_on_ME_monthly_EW.csv"

loadReturns :: IO (Frame Returns)
loadReturns = inCoreAoS returnsStream

main :: IO ()
main = do
  ms <- loadReturns
  let casted_ms = fmap (rcast @'[Timestamp, Hi10, Lo10]) ms
  let hi = calculateDrawdown @Hi10 casted_ms
  let low = calculateDrawdown @Lo10 casted_ms
  print $ take 4 (Fold.toList hi)
  print $ take 6 $ Fold.toList $ filterFrame (\r -> rgetField @Timestamp r < 195000) hi
  print $ F.fold F.minimum $ fmap (\r -> (rgetField @Drawdown r, rgetField @Timestamp r)) low
  print $ F.fold F.minimum $ fmap (\r -> (rgetField @Drawdown r, rgetField @Timestamp r)) hi
  print $ F.fold F.minimum $ L.filter (\(_, t) -> t > 194000) $ fmap (\r -> (rgetField @Drawdown r, rgetField @Timestamp r)) (Fold.toList low)
  print $ F.fold F.minimum $ L.filter (\(_, t) -> t > 194000) $ fmap (\r -> (rgetField @Drawdown r, rgetField @Timestamp r)) (Fold.toList hi)
  print $ F.fold F.minimum $ L.filter (\(_, t) -> t > 197500) $ fmap (\r -> (rgetField @Drawdown r, rgetField @Timestamp r)) (Fold.toList low)
  print $ F.fold F.minimum $ L.filter (\(_, t) -> t > 197500) $ fmap (\r -> (rgetField @Drawdown r, rgetField @Timestamp r)) (Fold.toList hi)
