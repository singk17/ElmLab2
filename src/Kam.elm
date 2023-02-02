module Kam exposing (shapes,update,Msg(..),Model,init)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)

import Core
import Bitwise

type Msg = None

type alias Model = {
  }

init : Model
init = {}


update : (Core.CoreMsg a b Msg d e) -> (Core.TimeData,Model) -> (Core.TimeData,Model)
update msg (timedata,model) = (timedata,model)


hex : Int -> Color
hex c =
  let
    r = c |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xff |> toFloat
    g = c |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xff |> toFloat
    b = c |> Bitwise.and 0xff |> toFloat
  in
    rgb r g b

uglyBlue = hex 0x2f1fdf
lightBlue = hex 0xa4abde

shapes : (Core.TimeData,Model) -> List (Shape (Core.CoreMsg a b Msg d e))
shapes model = [
    cornerCut
  ]


cornerCut =
  let
    r = rectangle 30 35
        |> filled uglyBlue
    t = triangle 7
        |> filled red
        |> rotate (degrees (10))
        |> move (-13,16.5)
  in
  subtract t r
    |> makeTransparent 0.98