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
                      , segmentedBarGraphSample : SegmentedBarGraphSample }
type alias SevenSegmentSample = { properties : SevenSegmentProperties
                                , style : SevenSegmentStyle
                                , isVisible : Bool
                                , pointIndexesText : String
                                , colonIndexesText : String
                                }
type alias SegmentedBarGraphSample = { isVisible : Bool
                                     , properties : SegmentedBarGraphProperties
                                     , style : SegmentedBarGraphStyle }


------ DEFAULT MODELS
-- model for the whole app
defaultAppState : AppState 
defaultAppState = { sevenSegmentSample  = defaultSevenSegmentSample
                  , segmentedBarGraphSample = defaultSegmentedBarGraphSample
                  }

defaultSevenSegmentSample : SevenSegmentSample
defaultSevenSegmentSample = { properties = defaultSevenSegmentProperties
                            , style = defaultSevenSegmentStyle
                            , isVisible = True
                            , pointIndexesText = String.join "," (List.map (\i -> toString i) defaultSevenSegmentProperties.pointIndexes)
                            , colonIndexesText = String.join "," (List.map (\i -> toString i) defaultSevenSegmentProperties.colonIndexes)
                            }
  
defaultSegmentedBarGraphSample : SegmentedBarGraphSample
defaultSegmentedBarGraphSample = { isVisible = True
                                 , properties = defaultSegmentedBarGraphProperties
                                 , style = defaultSegmentBarGraphStyle }


-- actions
type Action 
  = NoOp
  | SeventSegmentTextChange String
  | SeventSegmentPointsChange String
  | SeventSegmentColonsChange String
  | SegmentedBarGraphValueChange String


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
                                        , segmentedBarGraphView address appState.segmentedBarGraphSample ] |> toElement w h

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

--helpers
convertToInt : String -> Int
convertToInt digit = (Maybe.withDefault -1 (Result.toMaybe (String.toInt digit )))

