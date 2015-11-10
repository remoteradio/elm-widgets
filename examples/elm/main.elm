-- author: virgilio dato
module Example.Main where

import Window
import Html exposing (..)
import Html.Attributes exposing (..)

import Html.Widgets exposing (..)


main : Html
main = div [ style [("width","400px")] ] [ sevenSegment "0123 456789" [] ]