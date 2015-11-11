-- author: virgilio dato
module Html.Widgets (sevenSegment) where

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String

---- SEVEN SEGMENT WIDGET
-- creates sevent segments given string and integers
-- digits       - example 12345
-- pointIndexes - example [1, 2] -> would generate 1.2.345
-- colonIndexes - example [2, 3] -> would generate 12:3:45
sevenSegment : String -> List Int -> List Int -> List Attribute -> List Attribute -> Html
sevenSegment digits pointIndexes colonIndexes containerAttributes foregroundAttributes = 
  let containerWidth = 200
      containerHeight = 340
      digitLength = (String.length digits)
  in Svg.svg [ version "1.1",height "100%", width "100%", x "0", y "0",  viewBox ("0 0 " ++ (toString (containerWidth * digitLength)) ++ " " ++ (toString containerHeight)) ]
              ([ rect ([x "0", y "0", width (toString (containerWidth * digitLength)), height (toString (containerHeight ))] ++ containerAttributes) [ ] ] 
                ++
              (List.indexedMap (sevenSegmentDigit ( containerWidth, containerHeight) foregroundAttributes) (String.toList digits))
                ++
              (sevenSegmentPoints (containerWidth, containerHeight) pointIndexes foregroundAttributes)
                ++
              (seventSegmentColons (containerWidth, containerHeight) colonIndexes foregroundAttributes))

--seven segment widget digit
sevenSegmentDigit : (Int, Int) -> List Attribute -> Int -> Char  -> Svg
sevenSegmentDigit (width, height) foregroundAttributes index digit  =
  let transformAttribute = transform ("translate(" ++ (toString (width * index)) ++ " 0)")  
      newForegroundAttribute = foregroundAttributes ++ [ transformAttribute ]
      segmentA = sevenSegmentDigitPolygon " 39.6,  35.4   52.5,  22.1  145.0,  22.1  157.0,  35.4  145.0,  48.2   52.5,  48.2" newForegroundAttribute
      segmentB = sevenSegmentDigitPolygon "151.4,  53.1  164.3,  41.8  175.5,  53.1  175.5, 150.8  163.5, 163.2  151.4, 151.2" newForegroundAttribute
      segmentC = sevenSegmentDigitPolygon "163.5, 176.5  175.5, 187.8  175.5, 285.5  163.5, 296.7  151.4, 283.4  151.4, 188.6" newForegroundAttribute
      segmentD = sevenSegmentDigitPolygon "145.4, 291.1  157.0, 303.9  145.4, 316.0   52.9, 316     40.0, 305.2   52.1, 291.1" newForegroundAttribute
      segmentE = sevenSegmentDigitPolygon " 45.2, 284.2   33.6, 296.7   22.3, 284.2   22.3, 188.6   33.8, 176.5   45.2, 187.8" newForegroundAttribute
      segmentF = sevenSegmentDigitPolygon " 33.8, 163.2   22.3, 150.4   22.3,  53.9   33.8,  41.8   45.2,  53.9   47.3, 150  " newForegroundAttribute
      segmentG = sevenSegmentDigitPolygon " 39.6, 170     51.7, 156.8  146.2, 156.8  157.0, 170    145.8, 182.9   52.1, 182.9" newForegroundAttribute
      polygons =  case digit of
                    '0' -> 
                      [ segmentA
                      , segmentB
                      , segmentC
                      , segmentD
                      , segmentE
                      , segmentF ]
                    '1' -> 
                      [ segmentB
                      , segmentC ]
                    '2' ->
                      [ segmentA 
                      , segmentB
                      , segmentD
                      , segmentE
                      , segmentG ]
                    '3' ->
                      [ segmentA
                      , segmentB
                      , segmentC
                      , segmentD
                      , segmentG ]
                    '4' ->
                      [ segmentB
                      , segmentC
                      , segmentF
                      , segmentG ]
                    '5' ->
                      [ segmentA
                      , segmentC
                      , segmentD
                      , segmentF
                      , segmentG ]
                    '6' ->
                      [ segmentA
                      , segmentC
                      , segmentD
                      , segmentE
                      , segmentF
                      , segmentG ]
                    '7' ->
                      [ segmentA
                      , segmentB
                      , segmentC ]
                    '8' ->
                      [ segmentA
                      , segmentB
                      , segmentC
                      , segmentD
                      , segmentE
                      , segmentF
                      , segmentG ]
                    '9' ->
                      [ segmentA
                      , segmentB
                      , segmentC
                      , segmentD
                      , segmentF
                      , segmentG ]
                    ' ' -> [ ]
  in Svg.g [ ] polygons

sevenSegmentDigitPolygon : String -> List Attribute -> Svg
sevenSegmentDigitPolygon points' attributes =
  polygon ([ points points' ] ++ attributes) [ ]

sevenSegmentPoints : (Int, Int) -> List Int -> List Attribute -> List Svg
sevenSegmentPoints (width, height) indexes attributes =
  List.map (sevenSegmentPoint attributes width) indexes

sevenSegmentPoint :  List Attribute ->Int -> Int -> Svg
sevenSegmentPoint attributes containerWidth index =               
  Svg.g [ ] 
        [ circle (attributes ++ [ cx (toString ((index + 1) * containerWidth)), cy "300" , r "16" ] ) [ ] ]

seventSegmentColons : (Int, Int) -> List Int -> List Attribute -> List Svg
seventSegmentColons (width, height) indexes attributes =
  List.map (seventSegmentColon attributes width) indexes

seventSegmentColon :  List Attribute ->Int -> Int -> Svg
seventSegmentColon attributes containerWidth index =               
  Svg.g [ ] 
        [ circle (attributes ++ [ cx (toString ((index + 1) * containerWidth)), cy "110" , r "16" ] ) [ ]
        , circle (attributes ++ [ cx (toString ((index + 1) * containerWidth)), cy "230" , r "16" ] ) [ ] ]

segmentedBarGraph : Int -> Int -> Int -> Html
segmentedBarGraph currentValue maxValue segments =
  let barWidth = 10
      containerHeight = 340
  in  Svg.svg [ version "1.1",height "100%", width "100%", x "0", y "0",  viewBox ("0 0 " ++ (toString (barWidth * segments)) ++ " " ++ (toString containerHeight)) ]
              [ ]