{-# LANGUAGE
  Rank2Types,
  TypeFamilies
 #-}

module Plotting where

import qualified Data.Vector as Vector
import Statistics.Sample

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

errBars :: Double -> Vector.Vector Double -> (Double, Double)
errBars k xs = (mean, k * stdDev) where
  (mean, var) = meanVariance xs
  stdDev = sqrt var

anytimeEval :: ([(a,Double)] -> Double) -> [(a,Double)] -> [Int] -> [Double]
anytimeEval f xs ns = map eval (filter (<= length xs) ns) where
  eval n = f (take n xs)

anytimePlot :: String -> String -> [Int] -> [(String, [Double])] -> EC (Layout LogValue LogValue) ()
anytimePlot x_title y_title ns inputs = do
  let xLabelShow = map (show . ceiling)
  let yLabelShow = map show

  let generatePlot (algName, ys) = plot $ line algName $ [zip (map fromIntegral ns) (map LogValue ys)]

  sequence $ map generatePlot inputs

  layout_x_axis . laxis_generate .= (autoScaledLogAxis $ loga_labelf .~ xLabelShow $ def)
  layout_y_axis . laxis_generate .= (autoScaledLogAxis $ loga_labelf .~ yLabelShow $ def)

  layout_x_axis . laxis_title .= x_title
  layout_y_axis . laxis_title .= y_title

oneShotPlot :: String -> String -> [(String, [(Double, (Double, Double))])] -> EC (Layout LogValue LogValue) ()
oneShotPlot x_title y_title inputs = do
  let xLabelShow = map (show . ceiling)
  let yLabelShow = map show

  let processPoint (x, (y,dy)) = symErrPoint (LogValue x) (LogValue y) 0 (LogValue dy)
  let generatePlot (algName, ps) = do {
      plot $ points algName $ map (\(x, (y,_)) -> (LogValue x, LogValue y)) ps;
      plot $ return (def & plot_errbars_values .~ map processPoint ps)}

  sequence $ map generatePlot inputs

  layout_x_axis . laxis_generate .= (autoScaledLogAxis $ loga_labelf .~ xLabelShow $ def)
  layout_y_axis . laxis_generate .= (autoScaledLogAxis $ loga_labelf .~ yLabelShow $ def)

  layout_x_axis . laxis_title .= x_title
  layout_y_axis . laxis_title .= y_title
