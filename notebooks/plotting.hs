{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module Plotting where

import Graphics.Vega.VegaLite hiding (length, filter)
import IHaskell.Display.Hvega
import qualified Data.Text as T
import Control.Arrow (first, second)

barplot (xs, ys) =
    let enc = encoding
                    . position X [ PName "X", PmType Nominal]
                    . position Y [ PName "Y", PmType Quantitative ]

        dat = (dataFromColumns [ ] 
                    . dataColumn "X" (Strings xs)
                    . dataColumn "Y" (Numbers ys)) []

    in toVegaLite [ 
                dat,
                mark Bar []
                , enc []
                , width 200
                , height 200
                ]

-- predData = baseData . 
--   dataColumn "Outlier Prediction" 
--       (Numbers ((\(x, y) -> log (fromIntegral y / (fromIntegral x+1))) 
--         <$> take 9000 (countOutliers mhRuns)))
  
-- predEncoding = baseEncoding . color [ MName "Outlier Prediction", VL.MmType VL.Quantitative]
-- showPlot predEncoding predData

scatterplot ((xs, ys), cs) cE f mode =
    let enc = encoding
                    . position X [ PName "X", PmType Quantitative]
                    . position Y [ PName "Y", PmType Quantitative ]
                    . cE
                    
                    -- color [ MName "Outlier"]

        dat = (dataFromColumns [ ] 
                    . dataColumn "X" (Numbers xs)
                    . dataColumn "Y" (Numbers ys)
                    . dataColumn "Outlier" (f cs)
                    ) []

    in toVegaLite [ 
                dat,
                mark mode []
                , enc []
                , width 200
                , height 200
                ]

class Plottable a where
    plotVega :: a -> VegaLiteLab

instance Plottable [((Double,Double),T.Text)] where
    plotVega ls = vlShow $ scatterplot 
        (first unzip $ unzip ls)
        (color [ MName "Outlier"])
        (\cs -> (Strings (T.pack . show <$> cs)))
        Circle

instance Plottable [((Double,Double),Double)] where
    plotVega ls = vlShow $ scatterplot 
        (first unzip $ unzip (ls))
        (color [ MName "Outlier", MmType Quantitative])
        Numbers
        Circle

instance Plottable ([Double],(Double,Double)) where
    plotVega ls = 
        let cs = take (length $ fst ls) $ Prelude.repeat 1
            xs = fst ls
            (slope,intercept) = snd ls
            ys = (+intercept) . (*slope) <$> xs
        in vlShow $ scatterplot 
        
            ((xs, ys), cs)
            (color [ ])
            Numbers
            Line

        -- color [ MName "Outlier Prediction", VL.MmType VL.Quantitative]

-- instance Plottable [((Double, Double), Double)] where
--     plotVega ls = scatt


instance Plottable [(T.Text, Double)] where
    plotVega ls = vlShow $ barplot $ unzip ls




  
--         baseData = dataFromColumns [ ]
--   . dataColumn "X" (Numbers range)
--   . dataColumn "Y" (Numbers (fst <$> samples))
--   . dataColumn "Outlier" (Strings (T.pack . show . snd <$> samples))

-- baseEncoding = encoding
--                     . position X [ PName "X" ]
--                     . position Y [ PName "Y" ]
--                     . color [ MName "Outlier"]

-- showPlot enc dat = vlShow $ toVegaLite [ 
--             dat [],
--             mark Point []
--               , enc []
--               , width 200
--               , height 200
--               ]

-- showPlot baseEncoding baseData