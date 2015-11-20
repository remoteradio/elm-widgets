-- author: virgilio dato
module Html.Widgets (sevenSegment, defaultSevenSegmentProperties, defaultSevenSegmentStyle, SevenSegmentProperties, SevenSegmentStyle
                    ,segmentedBarGraph,defaultSegmentedBarGraphProperties,defaultSegmentBarGraphStyle,SegmentedBarGraphProperties,SegmentedBarGraphStyle
                    ,simulatedAnalogMeter,defaultSimulatedAnalogMeterProperties,defaultSimulatedAnalogMeterStyle,SimulatedAnalogMeterProperties,SimulatedAnalogMeterStyle
                    ,knob,defaultKnobProperties,defaultKnobStyle,KnobProperties,KnobStyle) where

import Svg exposing (..)
import Svg.Attributes exposing (..)
import String

---- ALIASES
-- model that defines the values for the controls
-- digits - the values displayed
-- pointIndexes - the indexes where the points are displayed
-- colonIndexes - the indexes where colons are displayed
type alias SevenSegmentProperties = { digits : String
                                    , pointIndexes : List Int
                                    , colonIndexes : List Int
                                    , isSlant : Bool }
-- model that defines the colors for the parts of each control
-- backgroundColor - color for the background
-- textColor - color for the values displayed
type alias SevenSegmentStyle =  { backgroundColor : String
                                , textColor : String }

-- model that defines the values for the controls
-- currentValue - current value of the meter
-- maxValue - the maximum value of the meter
-- segments - the number of segments that are goint to be displayed in the meter
type alias SegmentedBarGraphProperties =  { currentValue : Int
                                          , maxValue : Int
                                          , segments : Int
                                          , ranges : List MeterRange }

-- model that defines the range and what colors it will display
type alias SegmentedBarGraphRange = { color : String
                                    , minValue : Float
                                    , maxValue : Float }

-- model that defines the style for the graph
type alias SegmentedBarGraphStyle = { emptyColor : String
                                    , backgroundColor : String }


-- model that defines the value range of the meter
type alias SimulatedAnalogMeterProperties = { currentValue : Int
                                            , maxValue : Int
                                            , ranges : List MeterRange }

-- model that defines the range and what colors it will display
type alias MeterRange = { color : String
                        , minValue : Float
                        , maxValue : Float }


-- model that defines style of the meter
type alias SimulatedAnalogMeterStyle =  { foreColor : String
                                        , backgroundColor : String }

type alias KnobProperties = { segments : Int
                            , smallSegments : Int
                            , rangeAngleMin : Int
                            , rangeAngleMax : Int }
type alias KnobStyle = { }

---- ALIAS CONSTRUCTORS
defaultSevenSegmentProperties = { digits = "1234 4567890"
                                , pointIndexes = [ 7 ]
                                , colonIndexes = [ 1 ]
                                , isSlant = False
                                }
defaultSevenSegmentStyle =  { backgroundColor = "#000"
                            , textColor = "#0F0"}

defaultSegmentedBarGraphProperties : SegmentedBarGraphProperties
defaultSegmentedBarGraphProperties =  { currentValue = 70
                                      , maxValue = 100
                                      , segments = 50
                                      , ranges  = [ defaultMeterRangeOk, defaultMeterRangeAlert ] }

defaultMeterRangeOk : MeterRange
defaultMeterRangeOk = { color = "#0F0"
                                , minValue = 0
                                , maxValue = 50 }

defaultMeterRangeAlert : MeterRange
defaultMeterRangeAlert =  { color = "#F00"
                          , minValue = 50.00
                          , maxValue = 100 }

defaultSegmentBarGraphStyle = { emptyColor = "#444"
                              , backgroundColor = "#000" }

defaultSimulatedAnalogMeterProperties = { currentValue = 50
                                        , maxValue = 100
                                        , ranges  = [ defaultMeterRangeOk, defaultMeterRangeAlert ]}
defaultSimulatedAnalogMeterStyle =  { foreColor = "#fff"
                                    , backgroundColor = "#000"  }

defaultKnobProperties = { segments = 20 
                        , smallSegments = 120
                        , rangeAngleMin = 90
                        , rangeAngleMax = 30 }
defaultKnobStyle = { }

---- SEVEN SEGMENT WIDGET
-- creates seven segments properties and sevent segment styles
sevenSegment : SevenSegmentProperties -> SevenSegmentStyle -> Svg
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
  ---91.8,122.4 -76.6,109.1 15.9,109.1 25.6,122.4 11.3,135.2 -81.2,135.2
  --16.8,140.1 31.7,128.8 40.9,140.1 23.7,237.8 9.5,250.2 -0.5,238.2
  --7.2,263.5 17.2,274.8 0,372.5 -14,383.7 -23.8,370.4 -7,275.6
  ---31.1,378.1 -21.8,390.9 -35.5,403 -128,403 -139,392.2 -124.4,378.1
  ---130.1,371.2 -143.9,383.7 -153,371.2 -136.1,275.6 -122.5,263.5 -113.1,274.8
  ---120.2,250.2 -129.4,237.4 -112.4,140.9 -98.8,128.8 -89.5,140.9 -104.3,237
  ---115.6,257 -101.1,243.8 -6.6,243.8 1.8,257 -11.6,269.9 -105.3,269.9
      segmentA = sevenSegmentDigitPolygon " 39.6,  35.4   52.5,  22.1  145.0,  22.1  157.0,  35.4  145.0,  48.2   52.5,  48.2" newForegroundAttribute
      segmentB = sevenSegmentDigitPolygon "151.4,  53.1  164.3,  41.8  175.5,  53.1  175.5, 150.8  163.5, 163.2  151.4, 151.2" newForegroundAttribute
      segmentC = sevenSegmentDigitPolygon "163.5, 176.5  175.5, 187.8  175.5, 285.5  163.5, 296.7  151.4, 283.4  151.4, 188.6" newForegroundAttribute
      segmentD = sevenSegmentDigitPolygon "145.4, 291.1  157.0, 303.9  145.4, 316.0   52.9, 316     40.0, 305.2   52.1, 291.1" newForegroundAttribute
      segmentE = sevenSegmentDigitPolygon " 45.2, 284.2   33.6, 296.7   22.3, 284.2   22.3, 188.6   33.8, 176.5   45.2, 187.8" newForegroundAttribute
      segmentF = sevenSegmentDigitPolygon " 33.8, 163.2   22.3, 150.4   22.3,  53.9   33.8,  41.8   45.2,  53.9   47.3, 150  " newForegroundAttribute
      segmentG = sevenSegmentDigitPolygon " 39.6, 170     51.7, 156.8  146.2, 156.8  157.0, 170    145.8, 182.9   52.1, 182.9" newForegroundAttribute
      polygons =  case digit of
                    '0' -> [ segmentA, segmentB, segmentC, segmentD, segmentE, segmentF ]
                    '1' -> [ segmentB, segmentC ]
                    '2' -> [ segmentA, segmentB, segmentD, segmentE, segmentG ]
                    '3' -> [ segmentA, segmentB, segmentC, segmentD, segmentG ]
                    '4' -> [ segmentB, segmentC, segmentF, segmentG ]
                    '5' -> [ segmentA, segmentC, segmentD, segmentF, segmentG ]
                    '6' -> [ segmentA, segmentC, segmentD, segmentE, segmentF, segmentG ]
                    '7' -> [ segmentA, segmentB, segmentC ]
                    '8' -> [ segmentA, segmentB, segmentC, segmentD, segmentE, segmentF, segmentG ]
                    '9' -> [ segmentA, segmentB, segmentC, segmentD, segmentF, segmentG ]
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
segmentedBarGraph : SegmentedBarGraphProperties -> SegmentedBarGraphStyle -> Svg
segmentedBarGraph properties style =
  let barWidth = 100
      containerHeight = 340
  in  Svg.svg [ version "1.1",height "100%", width "100%", x "0", y "0",  viewBox ("0 0 " ++ (toString (barWidth * properties.segments)) ++ " " ++ (toString containerHeight)) ]
              (([ Svg.rect [ fill style.backgroundColor, width "100%", height "340" ] [] ])
                ++
              (List.map (segmentedBarGraphBar (barWidth, containerHeight) properties style) [0..(properties.segments - 1 )])
              )

-- bar element for segmented bar graph
segmentedBarGraphBar : (Int, Int) -> SegmentedBarGraphProperties -> SegmentedBarGraphStyle -> Int -> Svg
segmentedBarGraphBar (width', height') properties style index =
  let transformAttribute = transform ("translate(" ++ (toString ((width' * index) + 4)) ++ " 8)") 
      barValue = ((toFloat index) / (toFloat properties.segments)) * (toFloat properties.maxValue)
      ranges = properties.ranges
      getRange = List.filter (\r -> r.minValue <= barValue && barValue <= r.maxValue ) ranges
      rangeFound = List.take 1 getRange
      barColor = case rangeFound of
        [range] -> if (toFloat properties.currentValue) <= barValue then style.emptyColor else range.color
        _ -> style.emptyColor
  in rect ([ transformAttribute ] ++ [ class (toString barValue) , fill barColor, width (toString (width' - 8)), height (toString (height' - 16)) ]) []

-- analog meter (Volume Unit Metra)
simulatedAnalogMeter : SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeter properties style' = 
  let containerWidth = 400
      containerHeight = 200
  in svg  [ Svg.Attributes.style "background:#000", version "1.1",height "100%", width "100%", x "0", y "0",  viewBox ("0 0 " ++ (toString containerWidth) ++ " " ++ (toString containerHeight)) ]
          [ rect [ fill style'.backgroundColor, x "2" ,width (toString  containerWidth), height (toString  containerHeight)] [ ]
          , simulatedAnalogMeterBars properties style'
          , simulatedAnalogMeterLabels properties style'
          , simulatedAnalogMeterPointer properties style' ]

-- draws the meters for analog meter bars
simulatedAnalogMeterBars : SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeterBars properties style = 
  Svg.g [ ] [ simulatedAnalogMeterBar       0.0   114   -45 0     properties style
            , simulatedAnalogMeterSmallBar  25    97    -37 0.062 properties style
            , simulatedAnalogMeterSmallBar  50    79    -34 0.125 properties style
            , simulatedAnalogMeterSmallBar  75    67    -30 0.187 properties style
            , simulatedAnalogMeterBar       100   54    -24 0.249  properties style
            , simulatedAnalogMeterSmallBar  125   50    -18 0.312 properties style
            , simulatedAnalogMeterSmallBar  150   45    -11 0.375 properties style
            , simulatedAnalogMeterSmallBar  175   42    -6  0.437 properties style
            , simulatedAnalogMeterBar       200   38    -0  0.499 properties style
            , simulatedAnalogMeterSmallBar  225   42    6   0.562 properties style
            , simulatedAnalogMeterSmallBar  250   45    11  0.625 properties style
            , simulatedAnalogMeterSmallBar  275   50    18  0.687 properties style
            , simulatedAnalogMeterBar       300   54    24  0.75  properties style
            , simulatedAnalogMeterSmallBar  325   67    30  0.812 properties style
            , simulatedAnalogMeterSmallBar  350   79    34  0.875 properties style
            , simulatedAnalogMeterSmallBar  375   97    37  0.937 properties style
            , simulatedAnalogMeterBar       400   114   45  0.999 properties style ]

-- draws labels for analog meter
simulatedAnalogMeterLabels : SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeterLabels properties style = 
  Svg.g [ ] [ simulatedAnalogMeterLabel -10 110 -45 0     0     properties style
            , simulatedAnalogMeterLabel 97   47 -24 0.244 0.25  properties style
            , simulatedAnalogMeterLabel 202  30   0 0.49  0.50  properties style
            , simulatedAnalogMeterLabel 312  51  24 0.749 0.75  properties style
            , simulatedAnalogMeterLabel 416 114  45 0.99     1     properties style ]

-- draws label for analog meter
-- x' - x of element
-- y' - y of element
-- rotation - number of degrees it would be rotated
-- percentage - of value reached
-- labelPercentage label of value
-- properties - properties of simulated analog meter properties
-- style - properties of simulated analog style
simulatedAnalogMeterLabel : Float -> Float -> Float -> Float -> Float ->  SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeterLabel x' y' rotation percentage labelPercentage properties style = 
  let ranges = properties.ranges
      valueToCheck = percentage * (toFloat properties.maxValue)
      labelPercentageValue  = labelPercentage * (toFloat properties.maxValue)
      getRange = List.filter (\r -> r.minValue <= valueToCheck && valueToCheck <= r.maxValue ) ranges
      rangeFound = List.take 1 getRange
      foreColor = case rangeFound of
        [range] -> if (toFloat properties.currentValue) <= valueToCheck then style.foreColor else range.color
        _ -> style.foreColor
  in text'  [ x (toString x')
            , y (toString y')
            , fill foreColor
            , class (toString properties.currentValue)
            , textAnchor "middle"
            , transform ("rotate(" ++ (toString rotation) ++ " " ++ (toString x') ++ "," ++ (toString y') ++ ")") ] 
            [ text (toString labelPercentageValue) ]

-- draws bar for analog meter
-- x' - x of element
-- y' - y of element
-- rotation - number of degrees it would be rotated
-- percentage - of value reached
-- labelPercentage label of value
-- properties - properties of simulated analog meter properties
-- style - properties of simulated analog style
simulatedAnalogMeterBar : Float -> Float -> Float -> Float -> SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeterBar x' y' rotation percentage properties style = 
  let height' = 20
      width' = 8
      ranges = properties.ranges
      valueToCheck = percentage * (toFloat properties.maxValue)
      getRange = List.filter (\r -> r.minValue <= valueToCheck && valueToCheck <= r.maxValue ) ranges
      rangeFound = List.take 1 getRange
      foreColor = case rangeFound of
        [range] -> if (toFloat properties.currentValue) <= valueToCheck then style.foreColor else range.color
        _ -> style.foreColor
  in rect [ x (toString x')
          , y (toString y')
          , fill foreColor
          , width (toString width')
          , height (toString height')
          , transform ("rotate(" ++ (toString rotation) ++ " " ++ (toString (x' + ((width' - 10) / 2))) ++ "," ++ (toString (y' + (height' / 2))) ++ ")") ] [ ]

-- draws small bars for analog meter
-- x' - x of element
-- y' - y of element
-- rotation - number of degrees it would be rotated
-- percentage - of value reached
-- labelPercentage label of value
-- properties - properties of simulated analog meter properties
-- style - properties of simulated analog style
simulatedAnalogMeterSmallBar : Float -> Float -> Float -> Float -> SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeterSmallBar x' y' rotation percentage properties style = 
  let height' = 10
      width' = 6
      ranges = properties.ranges
      valueToCheck = percentage * (toFloat properties.maxValue)
      getRange = List.filter (\r -> r.minValue <= valueToCheck && valueToCheck <= r.maxValue ) ranges
      rangeFound = List.take 1 getRange
      foreColor = case rangeFound of
        [range] -> if (toFloat properties.currentValue) <= valueToCheck then style.foreColor else range.color
        _ -> style.foreColor
  in rect [ x (toString x')
          , y (toString y')
          , fill foreColor
          , width (toString width')
          , height (toString height')
          , transform ("rotate(" ++ (toString rotation) ++ " " ++ (toString (x' + ((width' - 10) / 2))) ++ "," ++ (toString (y' + (height' / 2))) ++ ")") ] [ ]

-- draws pointer for analog meter
-- properties - properties of simulated analog meter properties
-- style - properties of simulated analog style
simulatedAnalogMeterPointer : SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeterPointer properties style = 
  let percentage = clamp -50 50 ((((toFloat properties.currentValue) / (toFloat properties.maxValue) ) * 100) - 50)
  in Svg.g  [ transform ("rotate(" ++ (toString percentage) ++ " 204,300)") ]
            [ polygon [ points "199,300 203,62 205,62 209,300", fill style.foreColor ] [ ] ]


---- KNOBS
knob : KnobProperties -> KnobStyle -> Svg
knob properties style = 
  let containerWidth = 340
      containerHeight = 340
  -- get number of small segments, segments, 
      segments = properties.segments
      smallSegments = properties.smallSegments
  -- have center radius, bar width
      radius = 120
      barHeight = 10
      smallBarHeight = 5
      centerX = 340 / 2
      centerY = 340 / 2
  -- compute middle radius by adding small bar width
  -- compute large radius by adding bar width
      middleRadius = radius + smallBarHeight
      largeRadius = radius + barHeight
  -- deterine the angles to calculate store it on barAngle and smallBarAngle based on number of bars
      barAngle = 360 / (toFloat segments)
      smallBarAngle = 360 / (toFloat (segments + smallSegments))
  -- for each angle
  in Svg.svg  [ version "1.1"
              , class (toString barAngle)
              , height "100%"
              , width "100%"
              , x "0"
              , y "0"
              ,  viewBox ("0 0 " ++ (toString containerWidth) ++ " " ++ (toString containerHeight)) ] 
              ( (List.map (knobSegment barAngle radius largeRadius (centerX, centerY)) [0..segments])
                  ++
                (List.map (knobSegment smallBarAngle radius middleRadius (centerX, centerY)) [0..(smallSegments + segments)])
                      )

knobSegment : Float -> Float -> Float -> (Float,Float) -> Int -> Svg
knobSegment angle innerRadius outerRadius (x', y') index = 
  -- using index determine angle
  -- use that angle, radius 1 and center point to determine point position 2 xy
  -- use that angle, radius 2 and center point to determine point position 3 xy
  -- draw that line from position 1 and 2
             --int x = (int)(150 + radius * System.Math.Cos(angle));
             --int y = (int)(150 + radius * System.Math.Sin(angle));
  let indexedAngle = ((angle * (toFloat index)) * (pi / 180))
      (x1', y1') = (fromPolar (innerRadius, indexedAngle))
      (x2', y2') = (fromPolar (outerRadius, indexedAngle))
  in Svg.g [ ]  [ line  [ x1 (toString  (x1' + x'))
                        , y1 (toString (y1' + y'))
                        , x2 (toString (x2' + x'))
                        , y2 (toString (y2' + y'))
                        , class (toString (angle * (toFloat index)))
                        , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2" ] [ ] ]
    

