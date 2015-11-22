-- author: virgilio dato
module Example.Main where

import Window
import Graphics.Element exposing (Element, color)
import Graphics.Input exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Html.Widgets exposing (..)
import Svg.Attributes exposing (fill)
import Signal exposing (..)

import String exposing (..)

------ TYPES
type alias AppState = { sevenSegmentSample : SevenSegmentSample
                      , segmentedBarGraphSample : SegmentedBarGraphSample
                      , simulatedAnalogMeterSample : SimulatedAnalogMeterSample
                      , knobSample : KnobSample }

type alias SevenSegmentSample = { properties : SevenSegmentProperties
                                , style : SevenSegmentStyle
                                , isVisible : Bool
                                , pointIndexesText : String
                                , colonIndexesText : String }

type alias SegmentedBarGraphSample = { isVisible : Bool
                                     , properties : SegmentedBarGraphProperties
                                     , style : SegmentedBarGraphStyle }

type alias SimulatedAnalogMeterSample = { isVisible : Bool
                                        , properties : SimulatedAnalogMeterProperties
                                        , style : SimulatedAnalogMeterStyle }

type alias KnobSample = { isVisible : Bool 
                        , properties : KnobProperties 
                        , style : KnobStyle }


------ DEFAULT MODELS
-- model for the whole app
defaultAppState : AppState 
defaultAppState = { sevenSegmentSample  = defaultSevenSegmentSample
                  , segmentedBarGraphSample = defaultSegmentedBarGraphSample
                  , simulatedAnalogMeterSample = defaultSimulatedAnalogMeterSample
                  , knobSample = defaultKnobSample
                  }

defaultSevenSegmentSample : SevenSegmentSample
defaultSevenSegmentSample = { properties = defaultSevenSegmentProperties
                            , style = defaultSevenSegmentStyle
                            , isVisible = True
                            , pointIndexesText = String.join "," (List.map (\i -> toString i) defaultSevenSegmentProperties.pointIndexes)
                            , colonIndexesText = String.join "," (List.map (\i -> toString i) defaultSevenSegmentProperties.colonIndexes)
                            }
  
defaultSegmentedBarGraphSample : SegmentedBarGraphSample
defaultSegmentedBarGraphSample = { isVisible = False
                                 , properties = defaultSegmentedBarGraphProperties
                                 , style = defaultSegmentBarGraphStyle }

defaultSimulatedAnalogMeterSample : SimulatedAnalogMeterSample
defaultSimulatedAnalogMeterSample = { isVisible = False
                              , properties = defaultSimulatedAnalogMeterProperties
                              , style = defaultSimulatedAnalogMeterStyle }

defaultKnobSample : KnobSample
defaultKnobSample = { isVisible = True
                    , properties = defaultKnobProperties
                    , style = defaultKnobStyle }

-- actions
type Action 
  = NoOp
  | SeventSegmentTextChange String
  | SeventSegmentPointsChange String
  | SeventSegmentColonsChange String
  | SegmentedBarGraphValueChange String
  | SimulatedAnalogMeterValueChange String


-- logic when an update signal is emitted
update : Action -> AppState -> AppState
update action appState =
  case action of
    NoOp -> appState
    SeventSegmentTextChange value -> 
      let sevenSegmentSample' = appState.sevenSegmentSample 
          properties' = sevenSegmentSample'.properties
      in { appState | sevenSegmentSample <- { sevenSegmentSample' | properties <- { properties' | digits <- value } } }
    SeventSegmentPointsChange value ->
      let sevenSegmentSample' = appState.sevenSegmentSample
          properties' = sevenSegmentSample'.properties
          convert digit = (Maybe.withDefault -1 (Result.toMaybe (String.toInt digit )))
          pointIndexes' = (List.filter (\y -> y /= -1 ) (List.map (\x -> convert x ) (String.split "," value)))
      in { appState | sevenSegmentSample <- { sevenSegmentSample' | properties <- { properties' | pointIndexes <- pointIndexes' }                                                            , pointIndexesText <- value }}
    SeventSegmentColonsChange value ->
      let sevenSegmentSample' = appState.sevenSegmentSample
          properties' = sevenSegmentSample'.properties
          convert digit = (Maybe.withDefault -1 (Result.toMaybe (String.toInt digit )))
          colonIndexes' = (List.filter (\y -> y /= -1 ) (List.map (\x -> convert x ) (String.split "," value)))
      in { appState | sevenSegmentSample <- { sevenSegmentSample' | properties <- { properties' | colonIndexes <- colonIndexes' }
                                                                  , colonIndexesText <- value }}
    SegmentedBarGraphValueChange value ->
      let segmentedBarGraphSample' = appState.segmentedBarGraphSample
          properties' = segmentedBarGraphSample'.properties
      in { appState | segmentedBarGraphSample <- { segmentedBarGraphSample' | properties <- { properties' | currentValue <- convertToInt value } } }
    SimulatedAnalogMeterValueChange value ->
      --appState
      let simulatedAnalogMeterSample' = appState.simulatedAnalogMeterSample
          properties' = simulatedAnalogMeterSample'.properties
      in { appState | simulatedAnalogMeterSample <- { simulatedAnalogMeterSample' | properties <- { properties' | currentValue <- convertToInt value } } }
--- 


--- entry point
main : Signal Element
main =
  Signal.map2 (appView actions.address) appState Window.dimensions 

-- manage the appState of our application over time
appState : Signal AppState
appState =
  Signal.foldp update defaultAppState mergedActions

-- actions from user input
actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

mergedActions : Signal Action
mergedActions = Signal.mergeMany [ actions.signal ]  

appView :Address Action -> AppState -> (Int,Int) -> Element
appView address appState (w,h) = div [] [ sevenSegmentSampleView address appState.sevenSegmentSample
                                        , segmentedBarGraphView address appState.segmentedBarGraphSample
                                        , simulatedAnalogMeterView address appState.simulatedAnalogMeterSample
                                        , knobView address appState.knobSample ] |> toElement w h

sevenSegmentSampleView : Address Action -> SevenSegmentSample -> Html
sevenSegmentSampleView address sevenSegmentSample' =
  let properties = sevenSegmentSample'.properties
  in  div [ style [if sevenSegmentSample'.isVisible then ("","") else ("display","none")] ] 
          [ div [ ] [ text "SEVENT SEGMENT EXAMPLE" ]
          , div [ Html.Attributes.style [("width","400px"),("height", "68px")] ]
                [ sevenSegment sevenSegmentSample'.properties sevenSegmentSample'.style ]
          , div [ ] [ text "TEXT"
                    , input [ type' "text"
                            , value properties.digits
                            , on "input" targetValue (Signal.message address << SeventSegmentTextChange) ][ ] ]
          , div [ ] [ text "POINTS"
                    , input [ type' "text"
                            , value sevenSegmentSample'.pointIndexesText
                            , on "input" targetValue (Signal.message address << SeventSegmentPointsChange) ][ ] ]
          , div [ ] [ text "COLONS"
                    , input [ type' "text"
                            , value sevenSegmentSample'.colonIndexesText
                            , on "input" targetValue (Signal.message address << SeventSegmentColonsChange) ][ ] ]
          ]

--type alias SegmentedBarGraphProperties =  { currentValue : Int
--                                          , maxValue : Int
--                                          , segments : Int
--                                          , ranges : List SegmentedBarGraphRange }
segmentedBarGraphView : Address Action -> SegmentedBarGraphSample -> Html
segmentedBarGraphView address segmentedBarGraphSample =
  let properties = segmentedBarGraphSample.properties
  in  div [ style [if segmentedBarGraphSample.isVisible then ("","") else ("display","none")] ] 
          [ div [ ] [ text "SEGMENTED BAR GRAPH SAMPLE" ]
          , div [ Html.Attributes.style [("width","400px"),("height", "68px")] ]
                [ segmentedBarGraph segmentedBarGraphSample.properties segmentedBarGraphSample.style  ]
          , div [ ] [ text "VALUE"
                        , input [ type' "text"
                                , value (toString properties.currentValue)
                                , on "input" targetValue (Signal.message address << SegmentedBarGraphValueChange) ][ ] ] ]

simulatedAnalogMeterView : Address Action -> SimulatedAnalogMeterSample -> Html
simulatedAnalogMeterView address sample = 
  let properties = sample.properties
  in  div [ style [if sample.isVisible then ("","") else ("display","none")] ] 
          [ div [ ] [ text "SIMULATED ANALOG METER VIEW" ]
          , div [ Html.Attributes.style [("width","400px"),("height", "136px")] ]
                [ simulatedAnalogMeter sample.properties sample.style  ]
          , div [ ] [ text "VALUE"
                    , input [ type' "text" 
                            , value (toString properties.currentValue)
                            , on "input" targetValue (Signal.message address << SimulatedAnalogMeterValueChange) ] [ ] ] ]
 

knobView : Address Action -> KnobSample -> Html
knobView address sample =
  let properties = sample.properties
  in  div [ style [if sample.isVisible then ("","") else ("display","none")] ] 
          [ div [ ] [ text "KNOB VIEW"] 
          , div [ Html.Attributes.style [("width","400px"),("height", "400px")] ]
                [ knob sample.properties sample.style ]]


--helpers
convertToInt : String -> Int
convertToInt digit = (Maybe.withDefault 0 (Result.toMaybe (String.toInt digit )))

