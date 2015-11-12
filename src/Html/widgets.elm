-- author: virgilio dato
module Html.Widgets (sevenSegment
                    , defaultSevenSegmentProperties
                    , defaultSevenSegmentStyle
                    , SevenSegmentProperties
                    , SevenSegmentStyle
                    ,segmentedBarGraph
                    ,defaultSegmentedBarGraphProperties
                    ,SegmentedBarGraphProperties) where

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String

---- ALIASES
-- model that defines the values for the controls
type alias SevenSegmentProperties = { digits : String
                                    , pointIndexes : List Int
                                    , colonIndexes : List Int }
-- model that defines the colors for the parts of each control
type alias SevenSegmentStyle =  { backgroundColor : String
                                , textColor : String }

-- model that defines the values for the controls
type alias SegmentedBarGraphProperties =  { currentValue : Int
                                          , maxValue : Int
                                          , segments : Int }

---- ALIAS CONSTRUCTORS
defaultSevenSegmentProperties = { digits = "1234 4567890"
                                , pointIndexes = [ 7 ]
                                , colonIndexes = [ 1 ]
                                }
defaultSevenSegmentStyle =  { backgroundColor = "#000"
                            , textColor = "#0F0"}

defaultSegmentedBarGraphProperties : SegmentedBarGraphProperties
defaultSegmentedBarGraphProperties =  { currentValue = 0
                                      , maxValue = 100
                                      , segments = 10 }

---- SEVEN SEGMENT WIDGET
-- creates seven segments properties and sevent segment styles
sevenSegment : SevenSegmentProperties -> SevenSegmentStyle -> Html
sevenSegment properties style = 
  let containerWidth = 200
      containerHeight = 340
      digitLength = (String.length properties.digits)
  in svg [ version "1.1",height "100%", width "100%", x "0", y "0",  viewBox ("0 0 " ++ (toString (containerWidth * digitLength)) ++ " " ++ (toString containerHeight)) ]
              ([ rect [x "0", y "0", width (toString (containerWidth * digitLength)), height (toString (containerHeight )), fill style.backgroundColor ] [ ] ] 
                ++
              (List.indexedMap (sevenSegmentDigit ( containerWidth, containerHeight) style) (String.toList properties.digits))
                ++
              (sevenSegmentPoints (containerWidth, containerHeight) properties.pointIndexes style)
                ++
              (seventSegmentColons (containerWidth, containerHeight) properties.colonIndexes style))

--seven segment widget digit
sevenSegmentDigit : (Int, Int) -> SevenSegmentStyle -> Int -> Char  -> Svg
sevenSegmentDigit (width, height) style index digit  =
  let transformAttribute = transform ("translate(" ++ (toString (width * index)) ++ " 0)")  
      newForegroundAttribute = [ transformAttribute, fill style.textColor ]
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

-- element for a segment

sevenSegmentDigitPolygon : String -> List Attribute -> Svg
sevenSegmentDigitPolygon points' attributes =
  polygon ([ points points' ] ++ attributes) [ ]

-- element for a list of points

sevenSegmentPoints : (Int, Int) -> List Int -> SevenSegmentStyle -> List Svg
sevenSegmentPoints (width, height) indexes style =
  List.map (sevenSegmentPoint style width) indexes

-- element for a point
sevenSegmentPoint : SevenSegmentStyle ->Int -> Int -> Svg
sevenSegmentPoint style containerWidth index =               
  Svg.g [ ] 
        [ circle [ cx (toString ((index + 1) * containerWidth)), cy "300" , r "16", fill style.textColor ] [ ] ]

-- element for list of colons
seventSegmentColons : (Int, Int) -> List Int -> SevenSegmentStyle -> List Svg
seventSegmentColons (width, height) indexes style =
  List.map (seventSegmentColon style width) indexes

-- element for colon
seventSegmentColon : SevenSegmentStyle ->Int -> Int -> Svg
seventSegmentColon style containerWidth index =               
  Svg.g [ ] 
        [ circle [ cx (toString ((index + 1) * containerWidth)), cy "110" , r "16", fill style.textColor ] [ ]
        , circle [ cx (toString ((index + 1) * containerWidth)), cy "230" , r "16", fill style.textColor ] [ ] ]

---- SEGMENTED BAR GRAPH WIDGET
-- creates a segmented bar graph widget given the following
-- current value - current value of meter 
-- max value - maximum value of the meter
-- number of polygons that make up the value of the bar
segmentedBarGraph : SegmentedBarGraphProperties -> Html
segmentedBarGraph segmentedBarGraphProperties =
  let barWidth = 100
      containerHeight = 340
  in  Svg.svg [ version "1.1",height "100%", width "100%", x "0", y "0",  viewBox ("0 0 " ++ (toString (barWidth * segmentedBarGraphProperties.segments)) ++ " " ++ (toString containerHeight)) ]
              [ Svg.rect [ fill "#000", width "100%", height "340" ] [] ]

segmentedBarGraphBar : (Int, Int) -> Int -> Svg
segmentedBarGraphBar (width', height) index =
  let transformAttribute = transform ("translate(" ++ (toString (width' * index)) ++ " 0)")  
  in rect [] []
  --rect ([ transformAttribute ] ++ [ fill "#000", width "100%", height "340" ]) []