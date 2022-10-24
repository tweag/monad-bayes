{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Plotting where

import Control.Arrow (first, second)
import qualified Data.Text as T
import Graphics.Vega.VegaLite hiding (filter, length)
import IHaskell.Display.Hvega

hist (xs, ys) =
  let enc =
        encoding
          . position X [PName "X", PmType Quantitative]
          . position Y [PName "Y", PmType Quantitative]

      dat =
        ( dataFromColumns []
            . dataColumn "X" (Numbers xs)
            . dataColumn "Y" (Numbers ys)
        )
          []
   in toVegaLite
        [ dat,
          mark Bar [],
          enc [],
          width 400,
          height 400
        ]

barplot (xs, ys) =
  let enc =
        encoding
          . position X [PName "X", PmType Nominal]
          . position Y [PName "Y", PmType Quantitative]

      dat =
        ( dataFromColumns []
            . dataColumn "X" (Strings xs)
            . dataColumn "Y" (Numbers ys)
        )
          []
   in toVegaLite
        [ dat,
          mark Bar [],
          enc [],
          width 400,
          height 400
        ]

scatterplot ((xs, ys), cs) cE f mode =
  let enc =
        encoding
          . position X [PName "X", PmType Quantitative]
          . position Y [PName "Y", PmType Quantitative]
          . cE

      dat =
        ( dataFromColumns []
            . dataColumn "X" (Numbers xs)
            . dataColumn "Y" (Numbers ys)
            . dataColumn "Outlier" (f cs)
        )
          []
   in toVegaLite
        [ dat,
          mark mode [],
          enc [],
          width 400,
          height 400
        ]

class Plottable a where
  plot :: a -> VegaLiteLab

instance Plottable [((Double, Double), T.Text)] where
  plot ls =
    vlShow $
      scatterplot
        (first unzip $ unzip ls)
        (color [MName "Outlier"])
        (\cs -> (Strings (T.pack . show <$> cs)))
        Circle

instance Plottable [((Double, Double), Double)] where
  plot ls =
    vlShow $
      scatterplot
        (first unzip $ unzip (ls))
        ( color
            [ MName "Outlier",
              MmType Quantitative,
              MScale
                [ SScheme "viridis" []
                ]
            ]
        )
        Numbers
        Circle

instance Plottable ([Double], (Double, Double)) where
  plot ls =
    let cs = take (length $ fst ls) $ Prelude.repeat 1
        xs = fst ls
        (slope, intercept) = snd ls
        ys = (+ intercept) . (* slope) <$> xs
     in vlShow $
          scatterplot
            ((xs, ys), cs)
            (color [])
            Numbers
            Line

instance Plottable [(T.Text, Double)] where
  plot ls = vlShow $ barplot $ unzip ls

instance Plottable [(Double, Double)] where
  plot ls = vlShow $ hist $ unzip ls

instance Plottable ([Double], [Double]) where
  plot (xs, ys) =
    vlShow $
      scatterplot ((xs, ys), replicate (length xs) 1) (color []) Numbers Line

type Plot = VegaLiteLab
