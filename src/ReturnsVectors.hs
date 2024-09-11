module ReturnsVectors where

import qualified Data.Vector as V

getReturns :: V.Vector Double -> V.Vector Double
getReturns v = V.zipWith (\a b -> a / b - 1) (V.tail v) (V.init v)

getTotalReturns :: V.Vector Double -> Double
getTotalReturns = subtract 1 . product . V.map (+ 1)

maxDrawdown :: V.Vector Double -> Double
maxDrawdown v = V.maximum v - V.minimum v

type TimePeriod = Int

periodizeReturns :: TimePeriod -> V.Vector Double -> V.Vector Double
periodizeReturns t v = V.map ((fromIntegral t / fromIntegral (V.length v)) **) v

calmarRatio :: TimePeriod -> V.Vector Double -> Double
calmarRatio t v = getTotalReturns (periodizeReturns t v) / maxDrawdown v

type InitialValue = Double

convertToWealth :: InitialValue -> V.Vector Double -> V.Vector Double
convertToWealth = V.scanl (\c r -> c * (1 + r))

drawdown :: InitialValue -> V.Vector Double -> V.Vector (Double, Double)
drawdown i v = V.zip wealth peaks
  where
    wealth = convertToWealth i v
    peaks = V.scanl max 0 wealth
