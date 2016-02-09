module Html.Widgets (sevenSegment, defaultSevenSegmentProperties, defaultSevenSegmentStyle, SevenSegmentProperties, SevenSegmentStyle
                    ,segmentedBarGraph,defaultSegmentedBarGraphProperties,defaultSegmentBarGraphStyle,SegmentedBarGraphProperties,SegmentedBarGraphStyle
                    ,simulatedAnalogMeter,defaultSimulatedAnalogMeterProperties,defaultSimulatedAnalogMeterStyle,SimulatedAnalogMeterProperties,SimulatedAnalogMeterStyle
                    ,knob,defaultKnobProperties,defaultKnobStyle,KnobProperties,KnobStyle
                    ,defaultMeterRangeOk,defaultMeterRangeAlert,MeterRange) where
{-| Svg Widgets using Elm-html


# Seven Segment Widget
@docs sevenSegment,defaultSevenSegmentProperties,defaultSevenSegmentStyle, SevenSegmentProperties, SevenSegmentStyle

# Segmented Bar Graph Widget
@docs segmentedBarGraph, defaultSegmentedBarGraphProperties, defaultSegmentBarGraphStyle, SegmentedBarGraphProperties, SegmentedBarGraphStyle

# Simulated Analog Meter Widget
@docs simulatedAnalogMeter,defaultSimulatedAnalogMeterProperties,defaultSimulatedAnalogMeterStyle,SimulatedAnalogMeterProperties,SimulatedAnalogMeterStyle

# Knob Widget
@docs knob,defaultKnobProperties,defaultKnobStyle,KnobProperties,KnobStyle

# Widget Variables
@docs MeterRange, defaultMeterRangeOk, defaultMeterRangeAlert
-}


import Svg exposing (..)
import Svg.Attributes exposing (..)
import String

{-| Properties alias for Seven Segment Widget
-}
type alias SevenSegmentProperties = { digits : String
                                    , pointIndexes : List Int
                                    , colonIndexes : List Int
                                    , isSlant : Bool
                                    , isDimmedWhenOff : Bool
                                    }

{-| Style alias for Seven Segment Widget
-}
type alias SevenSegmentStyle =  { backgroundColor : String
                                , textColor : String }

{-| Property alias for Segmented Bar Graph Widget
-}
type alias SegmentedBarGraphProperties =  { currentValue : Int
                                          , maxValue : Int
                                          , segments : Int
                                          , ranges : List MeterRange }

{-| Style alias for Segmented Bar Graph Widget
-}
type alias SegmentedBarGraphStyle = { emptyColor : String
                                    , backgroundColor : String }


{-| Property alias for Simulated Analog Meter
-}
type alias SimulatedAnalogMeterProperties = { currentValue : Int
                                            , maxValue : Int
                                            , ranges : List MeterRange }

{-| Style alias for Simulated Analog Meter
-}
type alias SimulatedAnalogMeterStyle =  { foreColor : String
                                        , backgroundColor : String }

{-| Properties alias for Knob
-}
type alias KnobProperties = { value : Int
                            , maxValue : Int
                            , segments : Int
                            , smallSegments : Int
                            , rangeAngleMin : Float
                            , rangeAngleMax : Float }

{-| Style alias for Knob
-}
type alias KnobStyle = { segmentColor : String
                       , knobColor : String
                       , knobPointerColor : String }

{-| Range alias for widgets
-}
type alias MeterRange = { color : String
                        , minValue : Float
                        , maxValue : Float }


{-| Default properties for SevenSegmentProperties
-}
defaultSevenSegmentProperties : SevenSegmentProperties
defaultSevenSegmentProperties = { digits = "1234 4567890"
                                , pointIndexes = [ 7 ]
                                , colonIndexes = [ 1 ]
                                , isSlant = False
                                , isDimmedWhenOff = True
                                }

{-| Default style for SevenSegmentStyle
-}
defaultSevenSegmentStyle : SevenSegmentStyle
defaultSevenSegmentStyle =  { backgroundColor = "#000"
                            , textColor = "#0F0"}

{-| Default properties for SegmentedBarGraph
-}
defaultSegmentedBarGraphProperties : SegmentedBarGraphProperties
defaultSegmentedBarGraphProperties =  { currentValue = 70
                                      , maxValue = 100
                                      , segments = 50
                                      , ranges  = [ defaultMeterRangeOk, defaultMeterRangeAlert ] }

{-| Default style for SegmentedBarGraph
-}
defaultSegmentBarGraphStyle : SegmentedBarGraphStyle
defaultSegmentBarGraphStyle = { emptyColor = "#444"
                              , backgroundColor = "#000" }

{-| Default properties for SimulatedAnalogMeter
-}
defaultSimulatedAnalogMeterProperties : SimulatedAnalogMeterProperties
defaultSimulatedAnalogMeterProperties = { currentValue = 50
                                        , maxValue = 100
                                        , ranges  = [ defaultMeterRangeOk, defaultMeterRangeAlert ]}

{-| Default style for SimulatedAnalogMeter
-}
defaultSimulatedAnalogMeterStyle : SimulatedAnalogMeterStyle
defaultSimulatedAnalogMeterStyle =  { foreColor = "#fff"
                                    , backgroundColor = "#000"  }

{-| Default properties for Knob
-}
defaultKnobProperties : KnobProperties
defaultKnobProperties = { segments = 20
                        , smallSegments = 60
                        , rangeAngleMin = 160
                        , rangeAngleMax = 20
                        , value = 0
                        , maxValue = 100 }

{-| Default style for Knob
-}
defaultKnobStyle : KnobStyle
defaultKnobStyle =  { segmentColor = "#F44"
                    , knobColor = "#444"
                    , knobPointerColor = "#F44" }

{-| Default meter range 'Ok'
-}
defaultMeterRangeOk : MeterRange
defaultMeterRangeOk = { color = "#0F0"
                                , minValue = 0
                                , maxValue = 50 }

{-| Default meter range 'alert'
-}
defaultMeterRangeAlert : MeterRange
defaultMeterRangeAlert =  { color = "#F00"
                          , minValue = 50.00
                          , maxValue = 100 }


{-| Creates Seven Segment Widget
-}
sevenSegment : SevenSegmentProperties -> SevenSegmentStyle -> Svg
sevenSegment properties style =
  let containerWidth = 200
      containerHeight = 340
      digitLength = (String.length properties.digits)
  in svg [ version "1.1",height "100%", width "100%", x "0", y "0",  viewBox ("0 0 " ++ (toString (containerWidth * digitLength)) ++ " " ++ (toString containerHeight)) ]
              ([ rect [x "0", y "0", width (toString (containerWidth * digitLength)), height (toString (containerHeight )), fill style.backgroundColor ] [ ] ]
                ++
              (List.indexedMap (sevenSegmentDigit properties style ( containerWidth, containerHeight)) (String.toList properties.digits))
                ++
              (sevenSegmentPoints style (containerWidth, containerHeight) properties.pointIndexes)
                ++
              (seventSegmentColons style (containerWidth, containerHeight) properties.colonIndexes))

{-| Creates SevenSegmentDigit
-}
sevenSegmentDigit : SevenSegmentProperties -> SevenSegmentStyle -> (Int, Int) -> Int -> Char  -> Svg
sevenSegmentDigit properties style (width, height) index digit  =
  let transformAttribute = transform ("translate(" ++ (toString (width * index)) ++ " 0)")
      newForegroundAttribute = [ transformAttribute ]
      segmentA isOn = sevenSegmentDigitPolygon properties style " 39.6,  35.4   52.5,  22.1  145.0,  22.1  157.0,  35.4  145.0,  48.2   52.5,  48.2" newForegroundAttribute isOn
      segmentB isOn = sevenSegmentDigitPolygon properties style "151.4,  53.1  164.3,  41.8  175.5,  53.1  175.5, 150.8  163.5, 163.2  151.4, 151.2" newForegroundAttribute isOn
      segmentC isOn = sevenSegmentDigitPolygon properties style "163.5, 176.5  175.5, 187.8  175.5, 285.5  163.5, 296.7  151.4, 283.4  151.4, 188.6" newForegroundAttribute isOn
      segmentD isOn = sevenSegmentDigitPolygon properties style "145.4, 291.1  157.0, 303.9  145.4, 316.0   52.9, 316     40.0, 305.2   52.1, 291.1" newForegroundAttribute isOn
      segmentE isOn = sevenSegmentDigitPolygon properties style " 45.2, 284.2   33.6, 296.7   22.3, 284.2   22.3, 188.6   33.8, 176.5   45.2, 187.8" newForegroundAttribute isOn
      segmentF isOn = sevenSegmentDigitPolygon properties style " 33.8, 163.2   22.3, 150.4   22.3,  53.9   33.8,  41.8   45.2,  53.9   47.3, 150  " newForegroundAttribute isOn
      segmentG isOn = sevenSegmentDigitPolygon properties style " 39.6, 170     51.7, 156.8  146.2, 156.8  157.0, 170    145.8, 182.9   52.1, 182.9" newForegroundAttribute isOn
      segment a b c d e f g = [ segmentA a, segmentB b, segmentC c, segmentD d, segmentE e, segmentF f, segmentG g ]
      polygons =  case digit of
                    '0' -> segment True   True  True  True  True  True  False
                    '1' -> segment False  True  True  False False False False
                    '2' -> segment True   True  False True  True  False True
                    '3' -> segment True   True  True  True  False False True
                    '4' -> segment False  True  True  False False True  True
                    '5' -> segment True   False True  True  False True  True
                    '6' -> segment True   False True  True  True  True  True
                    '7' -> segment True   True  True  False False False False
                    '8' -> segment True   True  True  True  True  True  True
                    '9' -> segment True   True  True  True  False True True
                    ' ' -> [ ]
                    otherwise -> segment False False False False False False False
  in Svg.g [ ] polygons

{-| Creates Polygon for sevenSegmentDigit
-}
sevenSegmentDigitPolygon : SevenSegmentProperties -> SevenSegmentStyle -> String -> List Attribute -> Bool -> Svg
sevenSegmentDigitPolygon properties style points' attributes isOn =
  let element = if isOn then polygon ([ points points', fill style.textColor ] ++ attributes) [ ]
                else  if  properties.isDimmedWhenOff then Svg.g [ ] [ polygon ([ points points', fill style.textColor ] ++ attributes) [ ]
                                                                                , polygon ([ points points', fill "#000", opacity "0.8" ] ++ attributes) [ ] ]
                      else Svg.g [ ][ ]
  in element

{-| Creates Points for sevenSegment
-}
sevenSegmentPoints : SevenSegmentStyle -> (Int, Int) -> List Int -> List Svg
sevenSegmentPoints style (width, height) indexes =
  List.map (sevenSegmentPoint style width) indexes

{-| Creates point for sevenSegmentPoints
-}
sevenSegmentPoint : SevenSegmentStyle ->Int -> Int -> Svg
sevenSegmentPoint style containerWidth index =
  Svg.g [ ]
        [ circle [ cx (toString ((index + 1) * containerWidth)), cy "300" , r "16", fill style.textColor ] [ ] ]

{-| Creates colons for sevenSegment
-}
seventSegmentColons : SevenSegmentStyle -> (Int, Int) -> List Int -> List Svg
seventSegmentColons style (width, height) indexes  =
  List.map (seventSegmentColon style width) indexes

{-| Creates colon for sevenSegmentColons
-}
seventSegmentColon : SevenSegmentStyle ->Int -> Int -> Svg
seventSegmentColon style containerWidth index =
  Svg.g [ ]
        [ circle [ cx (toString ((index + 1) * containerWidth)), cy "110" , r "16", fill style.textColor ] [ ]
        , circle [ cx (toString ((index + 1) * containerWidth)), cy "230" , r "16", fill style.textColor ] [ ] ]

{-| Creates Segmented Bar Graph
-}
segmentedBarGraph : SegmentedBarGraphProperties -> SegmentedBarGraphStyle -> Svg
segmentedBarGraph properties style =
  let barWidth = 100
      containerHeight = 340
  in  Svg.svg [ version "1.1",height "100%", width "100%", x "0", y "0",  viewBox ("0 0 " ++ (toString (barWidth * properties.segments)) ++ " " ++ (toString containerHeight)) ]
              (([ Svg.rect [ fill style.backgroundColor, width "100%", height "340" ] [] ])
                ++
              (List.map (segmentedBarGraphBar (barWidth, containerHeight) properties style) [0..(properties.segments - 1 )])
              )

{-| Creates bar for SegmentedBarGraph
-}
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

{-| Creates Simulated Analog Meter Widget
-}
simulatedAnalogMeter : SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeter properties style' =
  let containerWidth = 400
      containerHeight = 200
  in svg  [ Svg.Attributes.style "background:#000", version "1.1",height "100%", width "100%", x "0", y "0",  viewBox ("0 0 " ++ (toString containerWidth) ++ " " ++ (toString containerHeight)) ]
          [ rect [ fill style'.backgroundColor, x "2" ,width (toString  containerWidth), height (toString  containerHeight)] [ ]
          , simulatedAnalogMeterBars properties style'
          , simulatedAnalogMeterLabels properties style'
          , simulatedAnalogMeterPointer properties style' ]

{-|  Creates bars for simulatedAnalogMeter
-}
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

{-| creates labels for simulatedAnalogMeter
-}
simulatedAnalogMeterLabels : SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeterLabels properties style =
  Svg.g [ ] [ simulatedAnalogMeterLabel -10 110 -45 0     0     properties style
            , simulatedAnalogMeterLabel 97   47 -24 0.244 0.25  properties style
            , simulatedAnalogMeterLabel 202  30   0 0.49  0.50  properties style
            , simulatedAnalogMeterLabel 312  51  24 0.749 0.75  properties style
            , simulatedAnalogMeterLabel 416 114  45 0.99     1     properties style ]

{-| creates label for simulatedAnalogMeterLabels
-}
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

{-| Creates bar for simulatedAnalogMeterBars
-}
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

{-| Creates small bar for simulatedAnalogMeterBars
-}
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

{-| Creates pointer for simulatedAnalogMeter
-}
simulatedAnalogMeterPointer : SimulatedAnalogMeterProperties -> SimulatedAnalogMeterStyle -> Svg
simulatedAnalogMeterPointer properties style =
  let percentage = clamp -50 50 ((((toFloat properties.currentValue) / (toFloat properties.maxValue) ) * 100) - 50)
  in Svg.g  [ transform ("rotate(" ++ (toString percentage) ++ " 204,300)") ]
            [ polygon [ points "199,300 203,62 205,62 209,300", fill style.foreColor ] [ ] ]

{-| Creates knob widget
-}
knob : KnobProperties -> KnobStyle -> Svg
knob properties style =
  let
  -- have center radius, bar width
    containerWidth = 340
    containerHeight = 340
    radius = 120
    barHeight = 15
    smallBarHeight = 7
    centerX = 340 / 2
    centerY = 340 / 2
  -- compute middle radius by adding small bar width
  -- compute large radius by adding bar width
    middleRadius = radius + smallBarHeight
    largeRadius = radius + barHeight

    segments = properties.segments
    smallSegments = properties.smallSegments
    range = if properties.rangeAngleMin >= properties.rangeAngleMax then ((360 - properties.rangeAngleMin) + properties.rangeAngleMax) else properties.rangeAngleMax - properties.rangeAngleMin
    barAngle = range / (toFloat segments)
    smallBarAngle = range / (toFloat (segments + smallSegments))
    labelValuePerUnit = (toFloat properties.maxValue) / (toFloat segments)
    labelValues = List.map (\s -> (round ((toFloat s) * labelValuePerUnit ))) [0..segments]
  -- compute angles for bars
    barAngles = List.map (\a -> (toFloat a)  * barAngle) [ 0..properties.segments ]
  -- compute angles for small bars
    barSmallAngles = List.map (\a -> (toFloat a)  * smallBarAngle) [ 0..(properties.smallSegments + properties.segments) ]
  in Svg.svg  [ version "1.1"
              , class (toString range)
              , height "100%"
              , width "100%"
              , x "0"
              , y "0"
              ,  viewBox ("0 0 " ++ (toString containerWidth) ++ " " ++ (toString containerHeight)) ]
              ((List.map2 (knobSegmentWithLabel properties style radius largeRadius (centerX, centerY)) barAngles labelValues)
                ++
              (List.map (knobSegment properties style radius middleRadius (centerX, centerY)) barSmallAngles)
                ++
              [ knobHandle properties style range (centerX, centerY) ]
              )

{-| Creates a knob handle for knob
-}
knobHandle : KnobProperties -> KnobStyle -> Float -> (Float, Float) -> Svg
knobHandle properties style range (x', y') =
  -- compute for the percentage of value over the maximum value
  let currentValuePercentage = (toFloat properties.value) / (toFloat properties.maxValue)
  -- compute the angle of the range value
      currentRangeValue = if properties.rangeAngleMin >= properties.rangeAngleMax then clamp properties.rangeAngleMin (360 + properties.rangeAngleMax) ((currentValuePercentage * range) + properties.rangeAngleMin)
                          else clamp properties.rangeAngleMin properties.rangeAngleMax ((currentValuePercentage * range) + properties.rangeAngleMin)
  in  Svg.g [ transform ("rotate(" ++ (toString currentRangeValue) ++ " " ++ (toString x') ++ "," ++ (toString y') ++ ")") ]
            [ circle [ cx (toString x'), cy (toString y'), r "112", fill style.knobColor ] [ ]
            , circle [ cx (toString (x' + 90)), cy (toString (y')), r "10", fill style.knobPointerColor ] [ ] ]

{-| Creates segments with label for knob
-}
knobSegmentWithLabel : KnobProperties -> KnobStyle -> Float -> Float ->(Float, Float) -> Float -> Int -> Svg
knobSegmentWithLabel properties style innerRadius outerRadius (x', y') angle labelValue =
  -- using index determine angle
  -- use that angle, radius 1 and center point to determine point position 2 xy
  -- use that angle, radius 2 and center point to determine point position 3 xy
  -- draw that line from position 1 and 2
             --int x = (int)(150 + radius * System.Math.Cos(angle));
             --int y = (int)(150 + radius * System.Math.Sin(angle));
  let
      labelRadius = outerRadius + 10
      piAngle = ((angle + properties.rangeAngleMin) * (pi / 180))

      --maxRange =
      --convertedIndexAngle = if  | properties.rangeAngleMin >= properties.rangeAngleMax ->
      --                              if | currentRangeValue <= properties.rangeAngleMin -> (currentRangeValue + properties.maxValue)
      --                                 | False -> 1
      --                          | otherwise -> clamp properties.rangeAngleMin properties.rangeAngleMax ((currentValuePercentage * range) + properties.rangeAngleMin)
      (x1', y1') = (fromPolar (innerRadius, piAngle))
      (x2', y2') = (fromPolar (outerRadius, piAngle))
      (x3', y3') = (fromPolar (labelRadius, piAngle))
  in Svg.g [ ]  [ line  [ x1 (toString  (x1' + x'))
                        , y1 (toString (y1' + y'))
                        , x2 (toString (x2' + x'))
                        , y2 (toString (y2' + y'))
                        , class (toString piAngle)
                        , Svg.Attributes.style ("stroke:" ++ style.segmentColor ++ ";stroke-width:2") ] [ ]
                , Svg.text' [ x (toString (x3' + x'))
                            , y (toString (y3' + y' + 4))
                            , textAnchor "middle"
                            ] [ text (toString labelValue) ] ]
{-| Creates segment for knob
-}
knobSegment : KnobProperties -> KnobStyle -> Float -> Float -> (Float, Float) -> Float -> Svg
knobSegment properties style innerRadius outerRadius (x', y') angle =
  -- using index determine angle
  -- use that angle, radius 1 and center point to determine point position 2 xy
  -- use that angle, radius 2 and center point to determine point position 3 xy
  -- draw that line from position 1 and 2
             --int x = (int)(150 + radius * System.Math.Cos(angle));
             --int y = (int)(150 + radius * System.Math.Sin(angle));
  let indexedAngle = ((angle + properties.rangeAngleMin) * (pi / 180))
      (x1', y1') = (fromPolar (innerRadius, indexedAngle))
      (x2', y2') = (fromPolar (outerRadius, indexedAngle))
  in Svg.g [ ]  [ line  [ x1 (toString  (x1' + x'))
                        , y1 (toString (y1' + y'))
                        , x2 (toString (x2' + x'))
                        , y2 (toString (y2' + y'))
                        , class (toString angle)
                        , Svg.Attributes.style ("stroke:" ++ style.segmentColor ++ ";stroke-width:2") ] [ ] ]
