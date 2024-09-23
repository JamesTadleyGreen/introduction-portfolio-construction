{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ReturnsData where

import qualified Control.Foldl as L
import qualified Data.Foldable as F
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
  print $ take 4 (F.toList hi)
  print $ take 6 $ F.toList $ filterFrame (\r -> rgetField @Timestamp r < 195000) hi
  print $ L.fold L.minimum (view drawdown <$> hi)
