module Viz exposing (view)
import Dict exposing (Dict)
import Shape exposing (linearCurve)
import Scale
import Axis exposing (tickCount)
import TypedSvg.Core exposing (Svg)
import TypedSvg as SVG exposing (g, svg)
import TypedSvg.Types exposing (px, Fill(..), Transform(..))
import TypedSvg.Attributes exposing (d, viewBox, transform)
import Path exposing (Path)
import Statistics
import Color
import Maybe.Extra
import List.Extra

width = 500
height = 500

sampleCount = 1024

lowerLimit = -10
upperLimit = 10

-- map width onto sample count
xScale = Scale.linear (0, width) (0, sampleCount)


scale yScale (x, y) = (
    Scale.convert xScale x
  , Scale.convert yScale y
  )

drawLine : Scale.ContinuousScale Float -> List (Maybe Float) -> Path
drawLine yScale values =
  let
    coords = values
      |> List.indexedMap (\idx val ->
        case val of
          Just v -> Just (toFloat idx, v)
          Nothing -> Nothing
      )
      |> List.map (Maybe.map (scale yScale))
  in
    Shape.line linearCurve coords

flatten2D : List (List a) -> List a
flatten2D list =
  List.foldr (++) [] list

makeYScale : Int -> List Timeline -> Scale.ContinuousScale Float
makeYScale tickCount timelines =
  let

    maximum =
      timelines
      |> List.map (
        Tuple.second
        >> List.map (Maybe.withDefault 0)
        >> List.maximum
        >> Maybe.withDefault 0
        )
      |> List.maximum
      |> Maybe.withDefault 10

    minimum =
      timelines
      |> List.map (
        Tuple.second
        >> List.map (Maybe.withDefault 0)
        >> List.minimum
        >> Maybe.withDefault 0
        )
      |> List.minimum
      |> Maybe.withDefault -10

    yScale = Scale.nice tickCount <| Scale.linear (0, height) (minimum, maximum)
  in
    yScale

type alias Timeline = (String, List (Maybe Float))

view : String -> List Timeline -> Svg msg
view svgId timelines =
  let
    -- STATIC
    -- stroke = TypedSvg.Attributes.stroke Color.black
    strokeWidth = TypedSvg.Attributes.strokeWidth (px 1)
    fill = TypedSvg.Attributes.fill FillNone
    tickCount = 5


    yScale = makeYScale tickCount timelines
    zeroPosition = scale yScale (512, 0)
      |> Tuple.first

    axisY =
          g [ transform [ Translate zeroPosition 0 ] ]
            [ Axis.left [ Axis.tickCount tickCount] yScale ]

    drawTimeline =
      Tuple.second
      >> (drawLine yScale)
      >> Path.toString
      >> d
      >> (\pathD -> SVG.path [strokeWidth, pathD, fill] [])

    paths = List.map drawTimeline timelines
    labels = List.map Tuple.first timelines

    makePath name p = g [TypedSvg.Attributes.class [name]] [p]

    svgPaths = List.map2 makePath labels paths

  in
    svg [TypedSvg.Attributes.class [ svgId ], viewBox (0 - 10) (0 - 10) (width + 20) (height + 20)] (svgPaths ++ [axisX, axisY])

axisX =
      g [ transform [ Translate 0 (height / 2) ] ]
        [ Axis.bottom [ Axis.ticks [0, 400, 800, 1024]] xScale ]

both : (Maybe a) -> (Maybe b) -> Maybe (a, b)
both a b =
  case a of
    Nothing -> Nothing
    Just a_ ->
      case b of
        Just b_ -> Just (a_, b_)
        Nothing -> Nothing
