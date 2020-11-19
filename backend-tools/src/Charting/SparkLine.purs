
module Charting.SparkLine where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Foldable (minimum, maximum)
import Data.Tuple (Tuple(..))
import Data.List (List, mapWithIndex)
import Data.List as List
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.HTML as HH

renderSparkline :: forall t1 t2. List (Maybe Number) -> HH.HTML t1 t2
renderSparkline xs =
 let reals = List.catMaybes xs
     vmin = minimum reals
     vmax = maximum reals
     normalize v = case (Tuple vmin vmax) of
        Tuple (Just v0) (Just v1) ->
          if v1 == v0
          then 5.0
          else (v - v0) / (v1 - v0)
        _ -> 5.0
     positionX idx = toNumber $ 305 - 3*idx
     positionY y = 20.0 * (1.0 - y)
     radius 0 = 3.0
     radius 1 = 2.0
     radius 2 = 1.5
     radius _ = 1.2

     mkSegment idx v1 v2 =
        SE.line [ SA.x1 $ positionX idx
                , SA.y1 $ positionY $ normalize v1
                , SA.x2 $ positionX (idx + 1)
                , SA.y2 $ positionY $ normalize v2
                , SA.stroke (Just $ SA.RGBA 20 20 20 0.8)
                , SA.strokeWidth 1.0
                ]


     mkVerticalBar idx =
        SE.line [ SA.x1 $ positionX idx
                , SA.y1 $ 0.0
                , SA.x2 $ positionX idx
                , SA.y2 $ 20.0
                , SA.stroke (Just $ SA.RGBA 120 20 20 0.8)
                , SA.strokeWidth 1.0
                ]

     render idx (Tuple (Just v1) (Just v2)) =
         mkSegment idx v1 v2

     render idx _ =
         mkVerticalBar idx
 in
 SE.svg [ SA.width 310.0
        , SA.height 20.0
        , SA.viewBox 0.0 0.0 310.0 20.0
        ]
    $ List.toUnfoldable
    $ mapWithIndex render
    $ List.zip xs (List.drop 1 xs)
