
module Charting.TimeSeries where

import Prelude
import Data.Foldable (minimum, maximum, sum)
import Data.Int (toNumber)
import Data.List as List
import Data.List (List, mapWithIndex)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE

renderChartTimeseries :: forall t1 t2. List (Maybe Number) -> HH.HTML t1 t2
renderChartTimeseries xs =
 let reals = List.catMaybes xs
     vmin = minimum reals
     vmax = maximum reals
     normalize v = case (Tuple vmin vmax) of
        Tuple (Just v0) (Just v1) ->
          if v1 == v0
          then 5.0
          else (v - v0) / (v1 - v0)
        _ -> 5.0
     positionX idx = toNumber $ 610 - 6*idx
     positionY y = 250.0 * (1.0 - y)
     radius 0 = 6.0
     radius 1 = 4.0
     radius 2 = 3.0
     radius _ = 2.4
     segments =
        List.toUnfoldable
        $ mapWithIndex (\idx (Tuple v1 v2) ->
            SE.g
              []
              [ SE.line
                  [ SA.x1 $ positionX idx
                  , SA.x2 $ positionX $ idx + 1
                  , SA.y1 $ positionY $ normalize v1
                  , SA.y2 $ positionY $ normalize v2
                  , SA.stroke (Just $ SA.RGBA 20 20 20 0.3)
                  , SA.strokeWidth 1.0
                  ]
               ])
        $ List.zip reals (List.drop 1 reals)
     points =
        List.toUnfoldable
        $ mapWithIndex (\idx v ->
            SE.g
              []
              [ SE.circle
                  [ SA.cx $ positionX idx
                  , SA.cy $ positionY $ normalize v
                  , SA.r $ radius idx
                  , SA.fill (Just $ if idx == 0 then SA.RGB 200 0 0 else SA.RGB 100 100 100)
                  ]
               ])
        $ reals
 in
 SE.svg [ SA.width 620.0
        , SA.height 250.0
        , SA.viewBox 0.0 0.0 620.0 250.0
        ]
        (segments <> points)

 
renderChartDiffTimeseries :: forall t1 t2. List (Maybe Number) -> HH.HTML t1 t2
renderChartDiffTimeseries xs =
 let samples = List.catMaybes xs
     reals = List.zipWith (\s0 s1 -> s1 - s0) (List.drop 1 samples) samples

     vmin = minimum reals
     vmax = maximum reals
     normalize v = case (Tuple vmin vmax) of
        Tuple (Just v0) (Just v1) ->
          if v1 == v0
          then 5.0
          else (v - v0) / (v1 - v0)
        _ -> 5.0
     positionX idx = toNumber $ 610 - 6*idx
     positionY y = 250.0 * (1.0 - y)
     radius 0 = 6.0
     radius 1 = 4.0
     radius 2 = 3.0
     radius _ = 2.4

     segments =
        List.toUnfoldable
        $ mapWithIndex (\idx (Tuple v1 v2) ->
            SE.g
              []
              [ SE.line
                  [ SA.x1 $ positionX idx
                  , SA.x2 $ positionX $ idx + 1
                  , SA.y1 $ positionY $ normalize v1
                  , SA.y2 $ positionY $ normalize v2
                  , SA.stroke (Just $ SA.RGBA 20 20 20 0.3)
                  , SA.strokeWidth 1.0
                  ]
               ])
        $ List.zip reals (List.drop 1 reals)
     points =
        List.toUnfoldable
        $ mapWithIndex (\idx v ->
            SE.g
              []
              [ SE.circle
                  [ SA.cx $ positionX idx
                  , SA.cy $ positionY $ normalize v
                  , SA.r $ radius idx
                  , SA.fill (Just $ if idx == 0 then SA.RGB 200 0 0 else SA.RGB 100 100 100)
                  ]
               ])
        $ reals
 in
 SE.svg [ SA.width 620.0
        , SA.height 250.0
        , SA.viewBox 0.0 0.0 620.0 250.0
        ]
    $ segments <> points

renderChartSmoothTimeseries :: forall t1 t2. List (Maybe Number) -> HH.HTML t1 t2
renderChartSmoothTimeseries xs =
 let samples = List.catMaybes xs
     average zs = (sum zs) / (toNumber $ List.length zs)
     reals = 
        List.drop 30
       $ map average
       $ mapWithIndex (\idx _ -> List.take 30 $ List.drop idx $ samples) samples

     vmin = minimum reals
     vmax = maximum reals
     normalize v = case (Tuple vmin vmax) of
        Tuple (Just v0) (Just v1) ->
          if v1 == v0
          then 5.0
          else (v - v0) / (v1 - v0)
        _ -> 5.0
     positionX idx = toNumber $ 610 - 6*idx
     positionY y = 250.0 * (1.0 - y)
     radius 0 = 6.0
     radius 1 = 4.0
     radius 2 = 3.0
     radius _ = 2.4
     segments =
        List.toUnfoldable
        $ mapWithIndex (\idx (Tuple v1 v2) ->
            SE.g
              []
              [ SE.line
                  [ SA.x1 $ positionX idx
                  , SA.x2 $ positionX $ idx + 1
                  , SA.y1 $ positionY $ normalize v1
                  , SA.y2 $ positionY $ normalize v2
                  , SA.stroke (Just $ SA.RGBA 20 20 20 0.3)
                  , SA.strokeWidth 1.0
                  ]
               ])
        $ List.zip reals (List.drop 1 reals)
     points =
        List.toUnfoldable
        $ mapWithIndex (\idx v ->
            SE.g
              []
              [ SE.circle
                  [ SA.cx $ positionX idx
                  , SA.cy $ positionY $ normalize v
                  , SA.r $ radius idx
                  , SA.fill (Just $ if idx == 0 then SA.RGB 200 0 0 else SA.RGB 100 100 100)
                  ]
               ])
        $ reals
 in
 SE.svg [ SA.width 620.0
        , SA.height 250.0
        , SA.viewBox 0.0 0.0 620.0 250.0
        ]
    $ segments <> points



