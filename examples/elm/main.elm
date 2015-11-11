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
type alias AppState = { sevenSegmentSample : SevenSegmentSample }
type alias SevenSegmentSample =   { text : String
                                  , pointIndexes : List Int
                                  , pointIndexesText : String
                                  , colonIndexes : List Int
                                  , colonIndexesText : String
                                  , isVisible : Bool
                                  }
type alias SegmentedBarGraphSample = { }


------ DEFAULT MODELS
-- model for the whole app
defaultAppState : AppState 
defaultAppState = { sevenSegmentSample  = defaultSevenSegmentSample }

defaultSevenSegmentSample : SevenSegmentSample
defaultSevenSegmentSample = { text = "0123456789"
                            , pointIndexes = [0, 3]
                            , pointIndexesText = "0,3"
                            , colonIndexes = [4, 5]
                            , colonIndexesText = "4,5"
                            , isVisible = True
                            }
  
segmentedBarGraphSample : SegmentedBarGraphSample
segmentedBarGraphSample = { }


-- actions
type Action 
  = NoOp
  | SeventSegmentTextChange String
  | SeventSegmentPointsChange String
  | SeventSegmentColonsChange String


-- logic when an update signal is emitted
update : Action -> AppState -> AppState
update action appState =
  case action of
    NoOp -> appState
    SeventSegmentTextChange value -> 
      let sevenSegmentSample' = appState.sevenSegmentSample 
      in { appState | sevenSegmentSample <- { sevenSegmentSample' | text <- value } }
    SeventSegmentPointsChange value ->
      let sevenSegmentSample' = appState.sevenSegmentSample
          convert digit = (Maybe.withDefault -1 (Result.toMaybe (String.toInt digit )))
          pointIndexes' = (List.filter (\y -> y /= -1 ) (List.map (\x -> convert x ) (String.split "," value)))
      in { appState | sevenSegmentSample <- { sevenSegmentSample' | pointIndexes <- pointIndexes'
                                                                  , pointIndexesText <- value }}
    SeventSegmentColonsChange value ->
      let sevenSegmentSample' = appState.sevenSegmentSample
          convert digit = (Maybe.withDefault -1 (Result.toMaybe (String.toInt digit )))
          colonIndexes' = (List.filter (\y -> y /= -1 ) (List.map (\x -> convert x ) (String.split "," value)))
      in { appState | sevenSegmentSample <- { sevenSegmentSample' | colonIndexes <- colonIndexes'
                                                                  , colonIndexesText <- value }}
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
appView address appState (w,h) = div [] [ sevenSegmentSampleView address appState.sevenSegmentSample ] |> toElement w h

sevenSegmentSampleView : Address Action -> SevenSegmentSample -> Html
sevenSegmentSampleView address sevenSegmentSample' =
  let containerAttributes   = [ fill "#000" ]
      foregroundAttributes  = [ fill "#7FD13B" ]
      
  in  div [ style [if sevenSegmentSample'.isVisible then ("","") else ("display","none")] ] 
          [ div [ ] [ text "SEVENT SEGMENT EXAMPLE" ]
          , div [ Html.Attributes.style [("width","400px"),("height", "68px")] ]
                [ sevenSegment sevenSegmentSample'.text sevenSegmentSample'.pointIndexes sevenSegmentSample'.colonIndexes containerAttributes foregroundAttributes ]
          , div [ ] [ text "TEXT"
                    , input [ type' "text"
                            , value sevenSegmentSample'.text
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


segmentedBarGraphView : Address Action -> SegmentedBarGraphSample -> Html
segmentedBarGraphView address sevenSegmentSample' =
  let containerAttributes   = [ fill "#000" ]
      foregroundAttributes  = [ fill "#7FD13B" ]
      
  in  div [ ] 
          [ ]


