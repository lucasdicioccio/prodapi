
module Charting.TimeSeries where

import Prelude
import Global (isFinite)
import Data.Foldable (minimum, maximum, sum)
import Data.Int (toNumber)
import Data.List as List
import Data.List (List, mapWithIndex)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE


type SegmentPref =
  { positionX :: Int -> Number
  , positionY :: Number -> Number
  , normalize :: Number -> Number
  , color :: Int -> SA.Color
  , width :: Int -> Number
  }

greylightA :: SA.Color
greylightA = SA.RGBA 20 20 20 0.2

bluelightA :: SA.Color
bluelightA = SA.RGBA 20 20 200 0.2

redlightA :: SA.Color
redlightA = SA.RGBA 200 20 20 0.2

greylight :: SA.Color
greylight = SA.RGB 20 20 20

bluelight :: SA.Color
bluelight = SA.RGB 20 20 200

redlight :: SA.Color
redlight = SA.RGB 200 20 20

palette :: Int -> SA.Color
palette 0 = greylight
palette 1 = redlight
palette 2 = bluelight
palette n = palette $ n `mod` 3

paletteA :: Int -> SA.Color
paletteA 0 = greylightA
paletteA 1 = redlightA
paletteA 2 = bluelightA
paletteA n = paletteA $ n `mod` 3

red :: SA.Color
red = SA.RGB 200 0 0

grey :: SA.Color
grey = SA.RGB 100 100 100

pointColor :: Int -> SA.Color
pointColor 0 = red
pointColor _ = grey

renderSegment :: forall t1 t2. SegmentPref -> Int -> Tuple Number Number -> HH.HTML t1 t2
renderSegment pref idx (Tuple v1 v2) =
  SE.g
    []
    [ SE.line
        [ SA.x1 $ pref.positionX $ idx
        , SA.x2 $ pref.positionX $ idx + 1
        , SA.y1 $ pref.positionY $ pref.normalize v1
        , SA.y2 $ pref.positionY $ pref.normalize v2
        , SA.stroke (Just $ pref.color idx)
        , SA.strokeWidth 1.0
        -- TODO: markerEnd
        ]
     ]

type PointPref =
  { positionX :: Int -> Number
  , positionY :: Number -> Number
  , normalize :: Number -> Number
  , radius :: Int -> Number
  , color :: Int -> SA.Color
  }

renderPoint :: forall t1 t2. PointPref -> Int -> Number -> HH.HTML t1 t2
renderPoint pref idx v =
  SE.g
    []
    [ SE.circle
        [ SA.cx $ pref.positionX idx
        , SA.cy $ pref.positionY $ pref.normalize v
        , SA.r $ pref.radius idx
        , SA.fill (Just $ pref.color idx)
        ]
     ]

renderVerticalBar :: forall t1 t2. SegmentPref -> Int -> Tuple Number Number -> HH.HTML t1 t2
renderVerticalBar pref idx (Tuple v1 v2) =
  SE.g
    []
    [ SE.line
        [ SA.x1 $ pref.positionX $ idx
        , SA.x2 $ pref.positionX $ idx
        , SA.y1 $ pref.positionY $ pref.normalize v1
        , SA.y2 $ pref.positionY $ pref.normalize v2
        , SA.stroke (Just $ pref.color idx)
        , SA.strokeWidth $ pref.width $ idx
        ]
     ]


renderChartTimeseries :: forall t1 t2. List (Maybe Number) -> HH.HTML t1 t2
renderChartTimeseries xs =
  renderMultiChartTimeseries (List.singleton xs)
 
renderChartDiffTimeseries :: forall t1 t2. List (Maybe Number) -> HH.HTML t1 t2
renderChartDiffTimeseries xs =
 let ys = List.zipWith f (List.drop 1 xs) xs
     f (Just s0) (Just s1) = Just $ s1 - s0
     f _ _ = Nothing
 in
 renderChartTimeseries ys

renderChartSmoothTimeseries :: forall t1 t2. List (Maybe Number) -> HH.HTML t1 t2
renderChartSmoothTimeseries xs =
 let samples = List.catMaybes xs
     average zs = (sum zs) / (toNumber $ List.length zs)
     reals = 
        List.drop 30
       $ map average
       $ mapWithIndex (\idx _ -> List.take 30 $ List.drop idx $ samples) samples
 in
 renderChartTimeseries $ map Just reals



renderMultiChartTimeseries :: forall t1 t2. List (List (Maybe Number)) -> HH.HTML t1 t2
renderMultiChartTimeseries xxs =
 let allReals = List.catMaybes $ List.concat xxs
     vmin = minimum allReals
     vmax = maximum allReals
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
     
     pref1 idx = { positionX: positionX
                 , positionY: positionY
                 , normalize: normalize
                 , color: const (palette idx)
                 , width: const 1.0
                 }
     pref2 idx = { positionX: positionX
                 , positionY: positionY
                 , normalize: normalize
                 , radius: radius
                 , color: const (palette idx)
                 }
     pref3 idx = { positionX: positionX
                 , positionY: identity
                 , normalize: identity
                 , color: const (paletteA idx)
                 , width: const 3.0
                 }


     renderMaybeSegment tsIdx ptIdx (Tuple (Just v1) (Just v2))
       | isFinite v1 && isFinite v2 =
         Just $ renderSegment (pref1 tsIdx) ptIdx (Tuple v1 v2)
     renderMaybeSegment _ _ _ = Nothing

     renderMaybePoint tsIdx ptIdx (Just v)
       | isFinite v =
          Just $ renderPoint (pref2 tsIdx) ptIdx v
     renderMaybePoint _ _ _ = Nothing

     renderMaybeOutage tsIdx ptIdx Nothing =
         Just $ renderVerticalBar (pref3 tsIdx) ptIdx (Tuple 0.0 250.0)
     renderMaybeOutage tsIdx ptIdx _ =
         Nothing

     segments tsIdx mreals =
        List.toUnfoldable
        $ List.catMaybes
        $ mapWithIndex (renderMaybeSegment tsIdx)
        $ List.zip mreals (List.drop 1 mreals)
     points tsIdx mreals =
        List.toUnfoldable
        $ List.catMaybes
        $ mapWithIndex (renderMaybePoint tsIdx)
        $ mreals
     outagebars tsIdx mreals =
        List.toUnfoldable
        $ List.catMaybes
        $ mapWithIndex (renderMaybeOutage tsIdx)
        $ mreals
 in
 SE.svg [ SA.width 620.0
        , SA.height 250.0
        , SA.viewBox 0.0 0.0 620.0 250.0
        ]
        ( List.toUnfoldable
          $ List.concat
          $ mapWithIndex (segments <> points <> outagebars) xxs
        )
